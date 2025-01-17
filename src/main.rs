use core::{
    config::{Class, Race, CLASSES, PRIME_STATS, RACES},
    format::Roman,
    lingo::{act_name, generate_name},
    mechanics::{Bar, Player, Simulation, StatsBuilder},
    Rand, SliceExt,
};
use std::path::Path;

use too::{
    layout::{Align, Align2, Axis, CrossAlign, Justify},
    renderer::{Border, Rgba},
    view::{DebugMode, Elements, Palette, Style as _, StyleOptions, Ui, ViewExt as _},
    views::{
        button, checkbox, label, list, progress, radio, separator, text_input, ButtonStyle,
        CheckboxStyle, Constrain, LabelStyle, List, Progress, ProgressStyle, RadioStyle,
        SeparatorStyle,
    },
    RunConfig,
};

mod core;

enum Screen {
    PlayerCreation {
        stats: StatsBuilder,
        player: Option<Player>,
    },
    PlayerSelect,

    Simulation {
        simulation: Simulation,
    },
}

struct App {
    screen: Screen,
    rng: core::Rand,
    roster: Vec<Player>,
}

impl App {
    fn rando_player(rng: &mut Rand, stats: &mut StatsBuilder) -> Player {
        Player::new(
            generate_name(6, rng),
            RACES.choice(rng).clone(),
            CLASSES.choice(rng).clone(),
            stats.roll(rng),
        )
    }

    fn load(path: impl AsRef<Path>) -> anyhow::Result<Self> {
        let mut rng = Rand::new();
        let mut stats = StatsBuilder::default();

        let mut this = App {
            screen: Screen::PlayerCreation {
                player: Some(Self::rando_player(&mut rng, &mut stats)),
                stats,
            },
            rng,
            roster: Vec::new(),
        };

        let path = path.as_ref();
        match std::fs::read(path)
            .ok()
            .map(|data| serde_json::from_slice::<Vec<Player>>(&data))
        {
            Some(Ok(roster)) => {
                this.roster = roster;
                this.screen = if this.roster.is_empty() {
                    let mut stats = StatsBuilder::default();
                    Screen::PlayerCreation {
                        player: Some(Self::rando_player(&mut this.rng, &mut stats)),
                        stats,
                    }
                } else {
                    Screen::PlayerSelect
                };
            }
            Some(Err(err)) => anyhow::bail!(err),
            None => {
                let mut stats = StatsBuilder::default();
                this.screen = Screen::PlayerCreation {
                    player: Some(Self::rando_player(&mut this.rng, &mut stats)),
                    stats,
                }
            }
        };

        Ok(this)
    }

    fn save(&self, path: impl AsRef<Path>) -> anyhow::Result<()> {
        let path = path.as_ref();
        let _ = std::fs::copy(path, path.with_extension("json.bak"));

        let mut roster = self.roster.clone();
        if let Screen::Simulation { simulation } = &self.screen {
            if let Some(pos) = roster
                .iter()
                .position(|c| c.birthday == simulation.player.birthday)
            {
                roster[pos] = simulation.player.clone();
            } else {
                roster.push(simulation.player.clone());
            }
        }

        std::fs::write(path, &serde_json::to_vec_pretty(&roster)?)?;
        Ok(())
    }
}

impl App {
    fn view(&mut self, ui: &Ui) {
        match &mut self.screen {
            Screen::PlayerCreation { .. } => self.player_creation(ui),
            Screen::PlayerSelect => self.player_select(ui),
            Screen::Simulation { simulation } => {
                simulation.tick(&mut self.rng);
                Game { simulation }.show(ui);
            }
        }
    }

    fn player_creation(&mut self, ui: &Ui) {
        let Screen::PlayerCreation { player, stats } = &mut self.screen else {
            return;
        };

        let mut creation = Creation::default();
        PlayerCreation {
            player: player.as_mut().unwrap(),
            stats,
            rng: &mut self.rng,
            creation: &mut creation,
            has_roster: !self.roster.is_empty(),
        }
        .show(ui);

        match creation {
            Creation::Accept => {
                self.roster.push(player.take().unwrap());
                self.screen = Screen::PlayerSelect
            }
            Creation::Cancel => self.screen = Screen::PlayerSelect {},
            Creation::Nothing => {}
        }
    }

    fn simulation_index(&mut self, index: usize) {
        let simulation = Simulation::new(self.roster[index].clone());
        self.screen = Screen::Simulation { simulation }
    }

    fn player_select(&mut self, ui: &Ui) {
        if self.roster.is_empty() {
            let mut stats = StatsBuilder::default();
            self.screen = Screen::PlayerCreation {
                player: Some(Self::rando_player(&mut self.rng, &mut stats)),
                stats,
            };
            return;
        }

        let mut selected = Selected::default();
        PlayerSelect {
            roster: &mut self.roster,
            selected: &mut selected,
        }
        .show(ui);

        match selected {
            Selected::Play { index } => self.simulation_index(index),
            Selected::CreateNew => {
                let mut stats = StatsBuilder::default();
                self.screen = Screen::PlayerCreation {
                    player: Some(Self::rando_player(&mut self.rng, &mut stats)),
                    stats,
                };
            }
            Selected::None => {}
        }
    }
}

struct Game<'a> {
    simulation: &'a mut Simulation,
}

impl<'a> Game<'a> {
    fn show(mut self, ui: &Ui) {
        ui.show_children(vertical_fill(), |ui| {
            ui.expand(|ui| {
                let columns = list()
                    .horizontal()
                    .gap(1)
                    .cross_align(CrossAlign::Stretch)
                    .justify(Justify::SpaceBetween);

                ui.show_children(columns, |ui| {
                    self.show_character_sheet(ui);
                    ui.expand(|ui| self.show_inventory(ui));
                    self.show_quest_log(ui);
                });
            });
            self.show_task_bar(ui);
        });
    }

    fn show_task_bar(&mut self, ui: &Ui) {
        ui.show_children(vertical_fill(), |ui| {
            ui.separator();
            if let Some(task) = &self.simulation.player.task {
                ui.label(&task.description);
            }
            ui.show(pacing_bar(&self.simulation.player.task_bar));
        });
    }

    fn show_character_sheet(&mut self, ui: &Ui) {
        ui.show_children(vertical_stretch(), |ui| {
            ui.vertical(|ui| {
                ui.horizontal(|ui| {
                    ui.expand(|ui| ui.label("Name"));
                    ui.label(&self.simulation.player.name);
                });
                ui.horizontal(|ui| {
                    ui.expand(|ui| ui.label("Race"));
                    ui.label(&self.simulation.player.race.name);
                });
                ui.horizontal(|ui| {
                    ui.expand(|ui| ui.label("Class"));
                    ui.label(&self.simulation.player.class.name);
                });
                ui.horizontal(|ui| {
                    ui.expand(|ui| ui.label("Level"));
                    ui.label(self.simulation.player.level)
                });
                ui.separator();

                for (stat, value) in self.simulation.player.stats.iter().take(PRIME_STATS.len()) {
                    ui.horizontal(|ui| {
                        ui.expand(|ui| ui.label(stat.as_str()));
                        ui.label(value)
                    });
                }
                ui.separator();

                for (stat, value) in self.simulation.player.stats.iter().skip(PRIME_STATS.len()) {
                    ui.horizontal(|ui| {
                        ui.expand(|ui| ui.label(stat.as_str()));
                        ui.label(value)
                    });
                }

                ui.separator();
                ui.show_children(vertical_fill(), |ui| {
                    ui.label("Experience");
                    ui.show(pacing_bar(&self.simulation.player.exp_bar));
                });
            });

            ui.vertical(|ui| {
                ui.separator();
                ui.label("Spell Book");
            });

            ui.expand(|ui| {
                ui.show_children(vertical_fill().scrollable(true), |ui| {
                    for (spell, level) in self.simulation.player.spell_book.iter() {
                        ui.horizontal(|ui| {
                            ui.expand(|ui| ui.label(spell));
                            ui.label(Roman::from_i32(level))
                        });
                    }
                });
            });
        });
    }

    fn show_inventory(&mut self, ui: &Ui) {
        ui.show_children(vertical_stretch(), |ui| {
            ui.show_children(vertical_stretch(), |ui| {
                for (equipment, kind) in self.simulation.player.equipment.iter() {
                    ui.horizontal(|ui| {
                        ui.expand(|ui| ui.label(equipment.as_str()));
                        ui.label(kind)
                    });
                }
            });

            ui.vertical(|ui| {
                ui.separator();
                ui.label("Inventory");
            });

            ui.expand(|ui| {
                ui.show_children(vertical_fill().scrollable(true), |ui| {
                    ui.horizontal(|ui| {
                        ui.expand(|ui| ui.label("Gold"));
                        ui.label(self.simulation.player.inventory.gold())
                    });

                    for (item, qty) in self.simulation.player.inventory.items() {
                        ui.horizontal(|ui| {
                            ui.expand(|ui| ui.label(item));
                            ui.label(qty)
                        });
                    }
                });
            });

            ui.show_children(vertical_stretch(), |ui| {
                ui.label("Encumbrance");
                ui.show(pacing_bar(&self.simulation.player.inventory.encumbrance));
            });
        });
    }

    fn show_quest_log(&mut self, ui: &Ui) {
        ui.show_children(vertical_stretch(), |ui| {
            let quest_book = &mut self.simulation.player.quest_book;
            let current = quest_book.act();

            ui.show_children(vertical_fill().scrollable(true), |ui| {
                let map_act = |act| (act < current, act_name(act));
                for (mut completed, name) in (0..=quest_book.act()).map(map_act) {
                    ui.show(checkbox(&mut completed, &name).class(pacing_check_box));
                }
            });
            ui.show_children(vertical_stretch(), |ui| {
                ui.show(pacing_bar(&quest_book.plot));
            });

            ui.expand(|ui| {
                ui.show_children(vertical_fill().scrollable(true), |ui| {
                    for (mut completed, name) in quest_book
                        .completed_quests()
                        .map(|q| (true, q))
                        .chain(quest_book.current_quest().map(|q| (false, q)))
                    {
                        ui.show(checkbox(&mut completed, &name).class(pacing_check_box));
                    }
                });
            });
            ui.show_children(vertical_stretch(), |ui| {
                ui.show(pacing_bar(&quest_book.quest));
            });
        });
    }
}

#[derive(Default)]
enum Selected {
    Play {
        index: usize,
    },
    CreateNew,
    #[default]
    None,
}

struct PlayerSelect<'a> {
    roster: &'a mut Vec<Player>,
    selected: &'a mut Selected,
}

impl<'a> PlayerSelect<'a> {
    fn format_time(secs: u64) -> String {
        let (h, m, s) = (secs / (60 * 60), (secs / 60) % 60, secs % 60);
        if h > 0 {
            format!("{h:02}:{m:02}:{s:02}")
        } else {
            format!("{m:02}:{s:02}")
        }
    }

    fn show(self, ui: &Ui) {
        let mut delete_character = None;

        ui.vertical(|ui| {
            ui.expand(|ui| {
                let resp = ui.show_children(list().vertical().scrollable(true).gap(1), |ui| {
                    for (i, player) in self.roster.iter().enumerate() {
                        ui.vertical(|ui| {
                            ui.horizontal(|ui| {
                                ui.expand(|ui| ui.label(&player.name));

                                ui.show(label("Level:").faint());
                                ui.label(player.level);

                                ui.show(label("Time played:").faint());
                                ui.label(Self::format_time(player.elapsed.round() as u64));

                                let delete = button("Delete").class(ButtonStyle::danger);
                                if ui.show(delete).clicked() {
                                    delete_character = Some(i);
                                }

                                let play = button("Play").class(ButtonStyle::info);
                                if ui.show(play).clicked() {
                                    *self.selected = Selected::Play { index: i }
                                }
                            });

                            ui.horizontal(|ui| {
                                ui.show(label("Birthday:").faint());
                                let birthday = player
                                    .birthday
                                    .format(&time::format_description::well_known::Rfc2822)
                                    .unwrap();
                                ui.label(birthday)
                            });

                            ui.horizontal(|ui| {
                                ui.show(label("Class:").faint());
                                ui.label(&*player.class.name);
                            });
                            ui.horizontal(|ui| {
                                ui.show(label("Race:").faint());
                                ui.label(&player.race.name);
                            });
                        });
                    }
                });

                ui.set_focus(resp.id());
            });

            ui.vertical(|ui| {
                ui.separator();
                if ui.button("New character").clicked() {
                    *self.selected = Selected::CreateNew
                }
            });
        });

        if let Some(index) = delete_character {
            self.roster.remove(index);
        }
    }
}

#[derive(Default)]
enum Creation {
    Accept,
    Cancel,
    #[default]
    Nothing,
}

struct PlayerCreation<'a> {
    player: &'a mut Player,
    stats: &'a mut StatsBuilder,
    rng: &'a mut Rand,
    creation: &'a mut Creation,
    has_roster: bool,
}

impl<'a> PlayerCreation<'a> {
    fn show(mut self, ui: &Ui) {
        self.show_meta_controls(ui);
        ui.vertical(|ui| {
            self.show_player_roll(ui);
            ui.show(separator().class(SeparatorStyle::thin_dashed));

            let stats_list = list()
                .horizontal()
                .justify(Justify::SpaceEvenly)
                .cross_align(CrossAlign::Stretch);
            ui.show_children(stats_list, |ui| {
                self.show_race_select(ui);
                self.show_class_select(ui);
                self.show_stats_roll(ui);
            });
        });
    }

    fn show_meta_controls(&mut self, ui: &Ui) {
        ui.aligned(Align2::RIGHT_TOP, |ui| {
            ui.horizontal(|ui| {
                self.show_navigation(ui);
            });
        });
    }

    fn show_navigation(&mut self, ui: &Ui) {
        if self.has_roster {
            let cancel = button("cancel")
                .class(ButtonStyle::danger)
                .text_horizontal_align(Align::CENTER);

            if ui.show(cancel).clicked() {
                *self.creation = Creation::Cancel
            }
        }

        let sold = button("sold!")
            .class(ButtonStyle::success)
            .text_horizontal_align(Align::CENTER)
            .disabled_if(self.player.name.trim().is_empty());

        if ui.show(sold).clicked() {
            *self.creation = Creation::Accept
        }
    }

    fn show_player_roll(&mut self, ui: &Ui) {
        ui.horizontal(|ui| {
            let resp = ui.constrain(Constrain::max_width(20), |ui| {
                let resp = ui.show(
                    text_input()
                        .initial(&self.player.name)
                        .placeholder("Enter a name"),
                );
                ui.set_focus(resp.id());
                resp
            });

            if resp.changed() {
                self.player.name = resp.data().to_string()
            }

            if ui.button("roll name").clicked() {
                self.player.name = generate_name(6, self.rng);
                resp.set_text(&self.player.name);
            }

            if ui.button("roll class").clicked() {
                self.player.race = RACES.choice(self.rng).clone();
                self.player.class = CLASSES.choice(self.rng).clone();
            }
        });
    }

    fn show_race_select(&mut self, ui: &Ui) {
        ui.expand(|ui| {
            ui.frame(Border::THICK, "Race", |ui| {
                ui.vertical(|ui| {
                    for race @ Race { name, .. } in RACES {
                        ui.show(
                            radio(race.clone(), &mut self.player.race, name)
                                .class(pacing_radio_class),
                        );
                    }
                });
            });
        });
    }

    fn show_class_select(&mut self, ui: &Ui) {
        ui.expand(|ui| {
            ui.frame(Border::THICK, "Class", |ui| {
                ui.vertical(|ui| {
                    for class @ Class { name, .. } in CLASSES {
                        ui.show(
                            radio(class.clone(), &mut self.player.class, name)
                                .class(pacing_radio_class),
                        );
                    }
                });
            });
        });
    }

    fn show_stats_roll(&mut self, ui: &Ui) {
        ui.frame(Border::THICK, "Stats", |ui| {
            ui.show_children(vertical_stretch(), |ui| {
                ui.vertical(|ui| {
                    for (stat, value) in self.player.stats.iter().take(PRIME_STATS.len()) {
                        ui.show_children(list().horizontal().gap(5), |ui| {
                            ui.expand(|ui| ui.label(stat.as_str()));
                            ui.label(value);
                        });
                    }

                    ui.show(separator().class(SeparatorStyle::thick));

                    for (stat, value) in self.player.stats.iter().skip(PRIME_STATS.len()) {
                        ui.show_children(list().horizontal().gap(5), |ui| {
                            ui.expand(|ui| ui.label(stat.as_str()));
                            ui.label(value);
                        });
                    }

                    ui.show(separator().class(SeparatorStyle::thick_dashed));

                    let total = self
                        .player
                        .stats
                        .iter()
                        .take(PRIME_STATS.len())
                        .fold(0, |a, (_, c)| a + c);

                    ui.show_children(list().horizontal().gap(5), |ui| {
                        const GREAT: usize = 4 * 18;
                        const GOOD: usize = 4 * 16;
                        const MEDIOCRE: usize = 4 * 15;
                        const POOR: usize = 4 * 13;

                        ui.expand(|ui| ui.label("Total"));

                        let foreground = match total {
                            total if total < POOR => ui.palette().danger,
                            total if total < MEDIOCRE => ui.palette().warning,
                            total if total < GOOD => ui.palette().foreground,
                            total if total < GREAT => ui.palette().info,
                            _ => ui.palette().success,
                        };

                        ui.show(label(total).style(LabelStyle { foreground }))
                    });
                });

                ui.expand_axis();

                ui.horizontal(|ui| {
                    let roll = button("roll");
                    if ui.show(roll).clicked() {
                        self.player.stats = self.stats.roll(self.rng)
                    }

                    let unroll = button("unroll?")
                        .class(ButtonStyle::danger)
                        .disabled_if(!self.stats.has_history());

                    if ui.show(unroll).clicked() {
                        self.player.stats = self.stats.unroll()
                    }
                });
            });
        });
    }
}

fn pacing_check_box(palette: &Palette, options: StyleOptions<bool>) -> CheckboxStyle {
    CheckboxStyle {
        text_color: if *options {
            palette.outline
        } else {
            palette.foreground
        },
        hovered_color: None,
        ..CheckboxStyle::ascii(palette, options)
    }
}

fn pacing_radio_class(palette: &Palette, selected: StyleOptions<bool>) -> RadioStyle {
    // RadioStyle::default(palette, selected)
    RadioStyle {
        selected: Some("🟢"),
        unselected: Some(if palette.is_dark() { "⚫" } else { "⚪" }),
        text_color: if *selected {
            palette.foreground
        } else {
            palette.outline
        },
        background: palette.background,
        selected_background: palette.background,
        hovered_text: Some(palette.contrast),
        hovered_background: None,
    }
}

fn pacing_bar(bar: &Bar) -> Progress {
    fn medium_progress(palette: &Palette, axis: StyleOptions<Axis>) -> ProgressStyle {
        ProgressStyle {
            filled: Elements::MEDIUM_RECT,
            unfilled: Elements::MEDIUM_RECT,
            ..ProgressStyle::default(palette, axis)
        }
    }
    progress(bar.pos)
        .range(0.0..=bar.max)
        .class(medium_progress)
}

fn vertical_fill() -> List {
    list().vertical().cross_align(CrossAlign::Fill)
}

fn vertical_stretch() -> List {
    list().vertical().cross_align(CrossAlign::Stretch)
}

fn main() -> anyhow::Result<()> {
    let mut app = App::load("pacing.json")?;
    too::application(
        RunConfig {
            palette: Palette {
                foreground: Rgba::hex("#DDD"),
                ..Palette::dark()
            },
            debug: DebugMode::PerFrame,
            // debug_anchor: Anchor2::LEFT_TOP,
            ..Default::default()
        },
        |ui| app.view(ui),
    )?;
    app.save("pacing.json")?;
    Ok(())
}
