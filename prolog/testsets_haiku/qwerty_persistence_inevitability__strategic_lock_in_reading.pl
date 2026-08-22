% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Lock-in via Manufacturer Cartel (Strategic Reading)
 *   domain: technology/political economy
 *
 * SUMMARY:
 *   The QWERTY keyboard layout, adopted by typewriter manufacturers in the
 *   1890s, persists as the dominant keyboard standard into the 21st century
 *   despite well-documented superior alternatives (Dvorak, Colemak). The
 *   strategic lock-in reading attributes this persistence not to path
 *   dependency or mechanical accident, but to deliberate manufacturer cartel
 *   strategy: Remington, Smith-Premier, Underwood, and allied firms
 *   standardized on QWERTY and created training partnerships with business
 *   colleges and typing schools to lock users into their machines. Once
 *   typists trained on QWERTY, switching manufacturers meant
 *   retraining—creating a network effect that benefited the cartel
 *   manufacturers and locked out competitors. This reading claims the
 *   constraint is tangled_rope: genuine coordination function
 *   (standardization reduced transaction costs and made labor portable)
 *   bundled with asymmetric extraction (cartel manufacturers collected
 *   monopoly rents via training lock-in). The sibling reading
 *   (path_dependency_reading) frames QWERTY as accidental path dependency—the
 *   outcome of mechanical constraints that became self-reinforcing through
 *   network effects without ongoing strategic cartel activity. These readings
 *   coexist: historians of technology contest whether the observed
 *   persistence reflects strategic cartel engineering (this reading) or
 *   emergent path dependency that needed no continuing strategy once
 *   established.
 *
 * KEY AGENTS:
 *   - Typewriter manufacturers (Remington, Smith-Premier, Underwood) — agenda-setters, collectors of lock-in rents via cartel standardization
 *   - Typists bearing ergonomic costs — victims, identity-locked by professional training and network externalities
 *   - Alternative-layout inventors (Dvorak, Colemak) — payers, innovations suppressed by lock-in
 *   - Typing school administrators — cartel partners, beneficiaries of exclusive training partnerships
 *   - Competing manufacturers — excluded by the cartel's control of training infrastructure
 *   - Economic historians (path-dependency camp) — observers, contesting the strategic-intent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Keyboard Layout Lock-in via Manufacturer Cartel (Strategic Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology/political economy").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d').
narrative_ontology:cs_kernel_codification('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', distributed).
narrative_ontology:cs_authority_grounding('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', extraction).
narrative_ontology:cs_reading_relation('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', foundational, qwerty_persistence_is_manufactured_lock_in).
narrative_ontology:cs_axiom_status(qwerty_persistence_is_manufactured_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', qwerty_persistence_is_manufactured_lock_in, empirically_contingent).
narrative_ontology:cs_axiom('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', secondary, cartel_training_partnerships_are_necessary_for_lock_in).
narrative_ontology:cs_axiom_status(cartel_training_partnerships_are_necessary_for_lock_in, holdable).
narrative_ontology:cs_axiom_grounding('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', cartel_training_partnerships_are_necessary_for_lock_in, empirically_contingent).
narrative_ontology:cs_reference_frame('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', cartel_enforced_standardization).
narrative_ontology:cs_drift_state('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', digital_era_contemporary, gap(stable, severe, false)).
narrative_ontology:cs_created_at('7d0b6c0f-2dd9-4db9-8e0f-1e318c9b2c6d', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_bearing_ergonomic_costs).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_denied_efficiency_gains).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_denied_efficiency_gains).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A coordinated set of typewriter manufacturers (Remington, Smith-Premier, Underwood, and allied firms) standardized on the QWERTY layout beginning in the 1890s. This was not inevitable accident but deliberate standardization to lock users into their machines—once typists trained on QWERTY, switching manufacturers meant retraining. The cartel maintained training partnerships with business colleges and typing schools, embedding QWERTY into curricula, making the standard self-reinforcing. They benefited directly from reduced competition: any manufacturer who deviated would lose the trained labor pool.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel, agenda_setter,
    institutional, generational, arbitrage, global).

% Typists trained on QWERTY bear the cumulative ergonomic costs of an inefficient layout: increased finger travel distance, higher rates of repetitive strain injury (RSI), fatigue, and reduced typing speed compared to alternatives like Dvorak. Their professional identity becomes fused with QWERTY competence; retraining to an alternative layout is costly in time and effort, and the network effect (all machines, all jobs, all training) makes exit economically irrational even for those who recognize the inefficiency. They are locked in by training investment and network externalities manufactured by the cartel.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_bearing_ergonomic_costs, payer,
    powerless, biographical, identity_locked, global).

% Inventors and innovators who designed more ergonomic layouts (Dvorak, Colemak, and others) found their innovations unable to gain traction because the cartel's lock-in made switching prohibitively expensive for the collective. Individual typists could not coordinate the switch, and manufacturers had no incentive to support alternatives when QWERTY was standardized. The lock-in suppressed the spread of superior technology.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_inventors, payer,
    moderate, biographical, trapped, global).

% Consumers and businesses benefited from standardization (all machines compatible, training transferable) but paid the cost of adopting an inefficient standard. They lost the efficiency gains that would have accrued if a superior layout had been adopted—a hidden cost borne as reduced productivity, increased injury, and higher capital expenditure on machines that could have been designed better. The benefit is real but asymmetrically distributed; the cost is diffuse and hard to quantify.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_denied_efficiency_gains, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_inevitability__strategic_lock_in_reading, consumers_denied_efficiency_gains, beneficiary).

% Manufacturers outside the cartel (or who came later) could not challenge QWERTY because the training partnership network and the installed base made deviation economically suicidal. A manufacturer who built Dvorak machines could not sell them—there were no trained Dvorak typists. The cartel's enforcement through control of training infrastructure locked out competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, competing_manufacturers, excluded,
    powerful, biographical, trapped, global).

% Typing schools benefited from cartel partnerships: the manufacturers provided machines and standardized curricula in exchange for exclusive training in QWERTY. The schools' business model became dependent on this arrangement. They had little incentive to teach alternatives because their revenue came from training typists for the installed base of QWERTY machines.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_administrators, beneficiary,
    moderate, biographical, constrained, national).

% Academic observers who frame QWERTY as path dependency—the product of historical accident (the arrangement of keys to prevent mechanical jamming in early machines) rather than deliberate strategy. This reading contests the strategic lock-in interpretation and holds that once QWERTY was adopted early, network effects made it self-reinforcing without requiring ongoing cartel activity.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians_path_dependency_camp, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized keyboard layout across competing manufacturers enabled compatible training, transportable skills, and predictable machine design. This genuine coordination reduced search costs for workers and made their labor more flexibly deployable across employers and geographic locations.
% TRANSFER_FUNCTION: Moves monopoly rents from the entire base of typists (and their employers, who must purchase machines conforming to QWERTY) to the cartel manufacturers and their training partners. The rents are extracted as: (1) higher machine prices than would prevail under competitive layout standards; (2) inability of superior layouts to emerge and displace QWERTY; (3) suppressed wages for typists (employers capture efficiency losses as reduced compensation); (4) costs borne collectively through health (RSI, fatigue) and time (slower typing).
% ABSENT_VOICES: Typists in the 1890s–1920s had no collective voice—unionization of clerical work lagged behind manufacturing, and typing training was controlled by the schools and manufacturers. Dvorak and other alternative-layout proponents lacked the manufacturing partnership networks the cartel controlled. Competing manufacturers who might have challenged the standard were locked out.
% DISAPPEARANCE_RATIONALE: If the cartel's lock-in had never been manufactured and QWERTY had been allowed to compete with alternatives on ergonomic grounds, a different layout or a mixed ecosystem might have emerged. Typists would have borne lower cumulative injury costs, productivity would be higher, and the text-input efficiency of the 20th and 21st centuries would reflect superior design. The world of work, health outcomes for clerical workers, and machine design would be substantially different.
% FOUNDING_PROBLEM: The immediate problem for early typewriter manufacturers was mechanical: key jamming in machines where typist speed exceeded the mechanical return speed. QWERTY was designed to slow down typing by separating frequently co-occurring letters, reducing jamming without requiring more expensive mechanisms. The manufacturers' problem was: how do we prevent competing standards and ensure all machines are compatible so we can control the market?
% FOUNDING_PROBLEM_CORROBORATION: The mechanical-jamming problem disappeared with the invention of the basket-shift mechanism in the 1890s and improved manufacturing by the early 1900s. Modern research (David, 1985; Liebowitz and Margolis, 1990; Koller, 2011) documents that QWERTY was no longer mechanically necessary after ~1905, yet persisted because cartel standardization and training-school partnerships made switching prohibitively expensive. The founders of the cartel (Remington, etc.) explicitly sought to standardize to prevent competition, as documented in company records and industry histories. CORROBORATION: Historians and economists outside the manufacturing industry attest the mechanical justification is gone; the cartel's own strategic intent is evident in archives.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 0.35 (early cartel formation, 1890s) to 0.68 (mature lock-in, 1950s) and plateaus. Early on, the cartel is engineering the lock-in (building partnerships, pushing QWERTY into curricula) but the network hasn't fully hardened—alternative layouts are still technically viable. By the 1920s–1940s, the training network is comprehensive and lock-in is pervasive; extractiveness peaks and stabilizes because network effects have become self-sustaining. Suppression rises similarly (0.55→0.72): early on, alternatives compete in technical merit and must be actively suppressed by cartel control of manufacturing and training. By mid-century, suppression is less active (the network effect does the work) but must remain high to prevent defection. Theater ratio rises gradually (0.25→0.41): the cartel's original narrative—mechanical necessity (jamming prevention)—becomes increasingly implausible after basket-shift invention (1890s–1900s), yet the constraint persists. By modern era, the only justification is 'network standardization,' which is genuine but obscures the cartel's role in engineering it. Theater ratio reflects this drift: early coordination story (real) gets replaced by standardization story (still real but shifts focus from cartel benefit to user convenience). The measurements are authored on a shared time grid spanning 0–120 years (industrial typewriter era through digital dominance).
 *
 * PERSPECTIVAL GAP:
 *   The cartel's seat (agenda-setter) perceives QWERTY as a successful coordination mechanism they created and stewarded—they enabled the mobility and standardization of clerical labor, a genuine coordination function. From this seat, the arrangement is rope or even justified mountain (natural standard). The typist seat perceives the same structure as manufactured lock-in—a prison of identity and network effects that suppresses better alternatives and extracts rents as ergonomic injury and lost efficiency. From this seat, the arrangement is snare-like tangled rope with victims. The engine's per-seat computation will reflect these divergent structural relationships: the cartel members see low extraction (beneficiaries, arbitrage exit), while typists see high extraction (victims, identity-locked exit). This divergence is the reading's core asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Typewriter manufacturers (institutional power, arbitrage exit) derive directionality near 0.0: they set the rules and benefit from the constraint's persistence—low d, low/negative extraction from their perspective. Typists (powerless, identity-locked exit) derive directionality near 1.0: they bear the costs (ergonomic, retraining barriers, suppressed alternatives) and have no real exit—high d, high effective extraction. Typing school administrators are ambiguous: they benefit from cartel partnerships (low d toward beneficiary) but are somewhat constrained by dependence on machines they don't control (elevated d). Alternative-layout inventors are trapped targets (high d, high extraction). The structural asymmetry is the story: the same arrangement that coordinates labor for the cartel extracts from the typists. This is the defining feature of tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical jamming in early typewriters) is structurally dead: modern keyboards have no jamming, the mechanical constraint disappeared after ~1905. Yet QWERTY persists, extracting at steady 0.68 from the interval midpoint onward. The question is: does the constraint persist because ongoing cartel enforcement maintains it, or because network effects have become self-sustaining and no cartel enforcement is needed? The measured theater_ratio (rising from 0.25→0.41) suggests the constraint's legitimating narrative has shifted from mechanical necessity to network coordination—a sign that mandatrophy may be partially resolved. However, high suppression (0.72) indicates active enforcement is still occurring: competing layouts are still technically superior but remain suppressed. The divergence between the sibling reading (path-dependency) and this reading hinges on whether the measured persistence is cartel-engineered (this reading's claim) or emergent path-dependency (the sibling's claim). This reading CLAIMS the persistence is strategically manufactured and maintained; the engine will compute whether the structural data support that claim. If suppression remains high despite the founding problem being dead, that supports the strategic-lock-in reading (active enforcement needed). If suppression drops but extraction persists, that supports path-dependency (self-sustaining network effects). The measurement series is designed to distinguish these.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_intent_vs_emergence,
    'Did the measured persistence of QWERTY result from ongoing cartel enforcement of lock-in, or from self-sustaining network effects that required no continuing strategic action after the initial standardization?',
    'Archival analysis of cartel communications and manufacturing strategy documents; measurement of suppression trajectory. If suppression remains high or increases, ongoing enforcement is indicated. If suppression decays while extraction persists, self-sustaining network effects are indicated. If alternative layouts are actively suppressed in modern markets, active enforcement is present. If they are suppressed only passively (no manufacturer bothers to support them), network effects dominate.',
    'If cartel enforcement is primary, the constraint is strategically engineered lock-in (supports this reading). If network effects are primary and no ongoing enforcement is required, the constraint transitions to path-dependent equilibrium (supports the sibling path-dependency reading). Classification divergence: snare vs. rope, or tangled-rope vs. piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_intent_vs_emergence, empirical, 'Whether QWERTY persistence is cartel-enforced lock-in or emergent network equilibrium.').

omega_variable(
    founding_problem_causation,
    'Did the mechanical jamming problem in early typewriters CAUSE the adoption of QWERTY, or was QWERTY selected for cartel-advantage reasons and the mechanical argument retrofitted as justification?',
    'Patent records, engineering documents, and manufacturer correspondence from the 1870s–1890s. If jamming prevention was the primary design constraint and QWERTY was selected for that reason, causation is mechanical. If manufacturers selected QWERTY for other reasons (e.g., simplicity, precedent, cartel coordination) and later justified it mechanically, causation is strategic.',
    'Mechanical causation supports the path-dependency reading (accident-driven origin). Strategic causation supports the lock-in reading (designed extraction). The narratives diverge at the origin and propagate through the mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_causation, empirical, 'Whether QWERTY''s adoption was caused by mechanical necessity or strategic cartel choice.').

omega_variable(
    training_network_counterfactual,
    'If the cartel had NOT created exclusive training partnerships with business colleges (i.e., if typing education had remained fragmented and non-standardized), would QWERTY have persisted as the dominant standard anyway?',
    'Historical counterfactual reasoning based on network-effects theory and comparative analysis of standards that emerged without coordinated training (e.g., railroad gauges, electrical current standards). If network effects alone would drive standardization, then the cartel''s training partnerships accelerated but did not create the lock-in. If training partnerships were necessary, the cartel''s strategy was essential.',
    'Training partnerships as NECESSARY → lock-in is cartel-engineered (this reading). Training partnerships as ACCELERATING but NOT NECESSARY → path-dependency is primary (sibling reading). This divergence is the core contest between the readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(training_network_counterfactual, conceptual, 'Whether the cartel''s training partnerships were necessary for lock-in or merely accelerated an inevitable network-effect equilibrium.').

omega_variable(
    ergonomic_cost_quantification,
    'What is the aggregate health and productivity cost borne by typists from using QWERTY instead of ergonomically superior alternatives (Dvorak, Colemak)?',
    'Epidemiological study of RSI prevalence in typist populations using QWERTY vs. alternatives; productivity studies comparing typing speed and accuracy. If substantial gaps are found (e.g., 20%+ productivity difference, 2–5x RSI rates), the extraction is large and real. If gaps are small, the lock-in''s victim costs may be smaller than claimed.',
    'Large gaps support the victim framing and the tangled-rope classification. Small gaps suggest the coordination benefits may offset the extraction costs, weakening the lock-in reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_quantification, empirical, 'Magnitude of health and productivity costs imposed by QWERTY lock-in on typists.').

omega_variable(
    kernel_reading_boundary,
    'Is QWERTY a single constraint viewed through different lenses (path-dependency vs. strategic lock-in), or are these genuinely different constraints (different ε values, different victim sets)?',
    'Application of the ε-invariance test: If changing the observable (what counts as ''persistence'') from ''the mechanical standard persists'' to ''the cartel''s extraction persists'' produces substantially different ε values, then two constraints are present, not one kernel. If ε remains stable, one constraint is viewed through two readings.',
    'One constraint, two readings → the readings coexist (current framing). Two constraints → they are separate stories, linked via network.affects_constraints (ε-invariance decomposition). This omega documents the framing choice and its boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether QWERTY persistence is one constraint with two readings or two structurally distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qwer_tr_t15, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(qwer_tr_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(qwer_tr_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 120, 0.41).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwer_be_t15, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(qwer_be_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(qwer_be_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qwer_su_t15, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(qwer_su_t30, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(qwer_su_t120, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 120, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% The QWERTY kernel decomposes into two constraint stories distinguished by locus of agency and persistence mechanism. This reading (strategic_lock_in_reading) attributes QWERTY's dominance to manufacturer cartel enforcement via training partnerships, treating persistence as engineered extraction. The sibling reading (path_dependency_reading) attributes it to emergent network effects after early adoption, treating persistence as equilibrium. Both readings share the referent (QWERTY's documented persistence) but assign different ε values and different victim/beneficiary sets. The readings coexist in the historiography; the engine computes per-seat divergence from the structural data, making explicit the empirical and conceptual differences between the two framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
