use core::{
    config::{Class, Race, CLASSES, PRIME_STATS, RACES},
    format::Roman,
    lingo::{act_name, generate_name},
    mechanics::{Bar, Player, Simulation, StatsBuilder},
    Rand, SliceExt,
};
use std::path::Path;

use too::{
    layout::{Align2, Axis},
    view::{
        self,
        views::{
            button::{button, ButtonStyle},
            checkbox::{checkbox, CheckboxStyle},
            expander::{separator, SeparatorStyle},
            label::{label, LabelStyle},
            list::list,
            progress::{progress, ProgressStyle},
            text_input::text_input,
            Constrain, CrossAlign, Justify, List, Progress,
        },
        Adhoc, Elements, Palette, Ui,
    },
    Border, Justification, Rgba,
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
                    // FIXME player select
                    Screen::PlayerSelect
                    // Screen::Simulation {
                    //     simulation: Simulation::new(this.roster.first().unwrap().clone()),
                    // }
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
            Screen::PlayerCreation { player, stats } => {
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

                    Creation::Cancel => {
                        let mut selected = Selected::default();
                        PlayerSelect {
                            roster: &mut self.roster,
                            selected: &mut selected,
                        }
                        .show(ui);

                        match selected {
                            Selected::Play { index } => {
                                self.screen = Screen::Simulation {
                                    simulation: Simulation::new(self.roster[index].clone()),
                                }
                            }
                            Selected::CreateNew => {
                                let mut stats = StatsBuilder::default();
                                self.screen = Screen::PlayerCreation {
                                    player: Some(Self::rando_player(&mut self.rng, &mut stats)),
                                    stats,
                                }
                            }
                            Selected::None => {}
                        }
                    }
                    Creation::Nothing => {}
                }
            }
            Screen::PlayerSelect if self.roster.is_empty() => {
                let mut stats = StatsBuilder::default();
                self.screen = Screen::PlayerCreation {
                    player: Some(Self::rando_player(&mut self.rng, &mut stats)),
                    stats,
                }
            }
            Screen::PlayerSelect => {
                let mut selected = Selected::default();
                PlayerSelect {
                    roster: &mut self.roster,
                    selected: &mut selected,
                }
                .show(ui);

                match selected {
                    Selected::Play { index } => {
                        self.screen = Screen::Simulation {
                            simulation: Simulation::new(self.roster[index].clone()),
                        }
                    }
                    Selected::CreateNew => {
                        let mut stats = StatsBuilder::default();
                        self.screen = Screen::PlayerCreation {
                            player: Some(Self::rando_player(&mut self.rng, &mut stats)),
                            stats,
                        }
                    }
                    Selected::None => {}
                }
            }
            Screen::Simulation { simulation } => Self::simulation(simulation, &mut self.rng, ui),
        }
    }

    fn simulation(simulation: &mut Simulation, rng: &mut Rand, ui: &Ui) {
        simulation.tick(rng);
        Game { simulation }.show(ui);
    }
}

fn vertical_fill() -> List {
    list().vertical().cross_align(CrossAlign::Fill)
}

fn vertical_stretch() -> List {
    list().vertical().cross_align(CrossAlign::Stretch)
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
                    let done = completed;
                    ui.adhoc(checkbox(&mut completed, &name).style(CheckboxStyle {
                        checked: "🗹",
                        unchecked: "☐",
                        text_color: if done {
                            ui.palette().outline
                        } else {
                            ui.palette().foreground
                        },
                        hovered_color: None,
                    }));
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
                        let done = completed;
                        ui.adhoc(checkbox(&mut completed, name).style(CheckboxStyle {
                            checked: "🗹",
                            unchecked: "☐",
                            text_color: if done {
                                ui.palette().outline
                            } else {
                                ui.palette().foreground
                            },
                            hovered_color: None,
                        }));
                    }
                });
            });
            ui.show_children(vertical_stretch(), |ui| {
                ui.show(pacing_bar(&quest_book.quest));
            });
        });
    }
}

fn pacing_bar(bar: &Bar) -> Progress {
    fn medium_progress(palette: &Palette, axis: Axis) -> ProgressStyle {
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
                                ui.label(
                                    player
                                        .birthday
                                        .format(&time::format_description::well_known::Rfc2822)
                                        .unwrap(),
                                )
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
        // TODO if we have a roster, have a 'cancel' button

        if self.has_roster {
            let cancel = button("cancel")
                .class(ButtonStyle::danger)
                .text_horizontal_align(Justification::CENTER);
            if ui.show(cancel).clicked() {
                *self.creation = Creation::Cancel
            }
        }

        let sold = button("sold!")
            .class(ButtonStyle::success)
            .text_horizontal_align(Justification::Center)
            .disabled_if(self.player.name.trim().is_empty()); // <-- this line

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
                        ui.adhoc(radio(race, &mut self.player.race, &name));
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
                        ui.adhoc(radio(class, &mut self.player.class, &name));
                    }
                });
            });
        });
    }

    fn show_stats_roll(&mut self, ui: &Ui) {
        ui.frame(Border::THICK, "Stats", |ui| {
            ui.show_children(list().vertical().cross_align(CrossAlign::Stretch), |ui| {
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

                ui.expander();

                ui.horizontal(|ui| {
                    let roll = button("roll");
                    if ui.show(roll).clicked() {
                        self.player.stats = self.stats.roll(self.rng)
                    }

                    let unroll = button("unroll?")
                        .class(ButtonStyle::danger)
                        .disabled_if(!self.stats.has_history()); // <-- this line

                    if ui.show(unroll).clicked() {
                        self.player.stats = self.stats.unroll()
                    }
                });
            });
        });
    }
}

struct Radio<'a, V> {
    item: &'a V,
    value: &'a mut V,
    label: &'a str,
}

impl<'v, V> Adhoc<'v> for Radio<'v, V>
where
    V: PartialEq + Clone,
{
    type Output = ();
    fn show(self, ui: &Ui) -> Self::Output {
        let resp = ui.mouse_area(|ui| {
            ui.horizontal(|ui| {
                let checked = if *self.item == *self.value {
                    '🟢'
                } else if ui.palette().is_dark() {
                    '⚫'
                } else {
                    '⚪'
                };
                ui.label(checked);

                let foreground = if ui.is_parent_hovered() {
                    ui.palette().contrast
                } else if *self.item == *self.value {
                    ui.palette().foreground
                } else {
                    ui.palette().outline
                };

                ui.show(label(self.label).style(LabelStyle { foreground }))
            });
        });

        if resp.flatten_left().clicked() {
            *self.value = self.item.clone();
        }
    }
}

fn radio<'a, V>(item: &'a V, value: &'a mut V, label: &'a str) -> Radio<'a, V>
where
    V: PartialEq + Clone,
{
    Radio { item, value, label }
}

fn main() -> anyhow::Result<()> {
    let mut app = App::load("pacing.json")?;
    view::application(
        || Palette {
            foreground: Rgba::hex("#CCC"),
            ..Palette::dark()
        },
        |ui| app.view(ui),
    )?;
    app.save("pacing.json")?;
    Ok(())
}
