% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Print Cost Collapse as Structural Inevitability of Religious Rupture (Technological-Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological_determinism_reading of the
 *   technology_reformation_causality kernel: the printing press as a fixed
 *   physical-economic constraint that made the Reformation inevitable by
 *   collapsing the cost of vernacular scripture distribution, with reformers
 *   as downstream adapters of a possibility the technology created. The
 *   claim/metric gap is deliberate and load-bearing: the reading CLAIMS
 *   mountain (print economics as natural-law-like substrate requiring no
 *   enforcement), while the authored metrics describe the structure honestly
 *   — low ongoing extraction, near-zero theater, high accessibility collapse,
 *   but non-trivial resistance (0.45) reflecting organized
 *   counter-mobilization (censorship indices, licensing regimes, vernacular
 *   bans) that a genuine natural law would not meet. Beneficiaries are
 *   declared INTENTIONALLY to trigger false-summit evaluation: the
 *   inevitability framing serves identifiable interpretive interests
 *   (media-determinist scholarship, Whig Protestant historiography, modern
 *   techno-determinist rhetoric), and the schema-required omega documents the
 *   natural-law-versus-constructed ambiguity. FAMILY DECOMPOSITION
 *   (epsilon-invariance): the colloquial label 'print caused the Reformation'
 *   decomposes into three structurally distinct constraints. This file
 *   locates operative force in the cost curve itself (low epsilon, 0.18,
 *   derived from production-cost reduction as the reading specifies);
 *   technology_reformation_causality__beneficiary_agency_reading relocates
 *   causation into strategic deployment by reformers and printers (moderate,
 *   targeted epsilon);
 *   technology_reformation_causality__co_constitution_reading distributes
 *   causation across mutual shaping (lowest attributable epsilon, least
 *   settled classification). Each file carries its own beneficiaries,
 *   victims, and epsilon; the network edges express family membership, not
 *   endorsement.
 *
 * KEY AGENTS:
 *   - - commercial_printers: Primary beneficiary with agenda-setting power (organized/arbitrage) — owns the reproduction capacity and decides what gets multiplied; serves all confessions indifferently
 *   - - vernacular_lay_readers: Mass beneficiary (powerless/constrained) — receives price surplus on scripture access; no voice in the system
 *   - - protestant_reformers: Beneficiary-agenda_setter (organized/identity_locked) — supplies the content the channel carries; committed past the point of exit
 *   - - scribal_workshops: Primary target (moderate/trapped) — bears the income destruction of the cost collapse
 *   - - ecclesiastical_gatekeepers: Secondary target (institutional/constrained) — bears dissolution of the information-scarcity position their authority monetized
 *   - - territorial_princes: Situational beneficiary (powerful/constrained) — converts print capability into administrative and jurisdictional reach
 *   - - media_historians: Analytical observer (analytical/analytical) — holds the comparative record that tests the inevitability premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.18).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.18).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Print Cost Collapse as Structural Inevitability of Religious Rupture (Technological-Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '882cb707-2a4c-46ae-af2d-684d00807d69').
narrative_ontology:cs_kernel_codification('882cb707-2a4c-46ae-af2d-684d00807d69', formalized).
narrative_ontology:cs_authority_grounding('882cb707-2a4c-46ae-af2d-684d00807d69', expertise).
narrative_ontology:cs_interpretation_layer_present('882cb707-2a4c-46ae-af2d-684d00807d69').
narrative_ontology:cs_reading_relation('882cb707-2a4c-46ae-af2d-684d00807d69', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('882cb707-2a4c-46ae-af2d-684d00807d69', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('882cb707-2a4c-46ae-af2d-684d00807d69', foundational, print_economics_determine_religious_outcomes).
narrative_ontology:cs_axiom_status(print_economics_determine_religious_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('882cb707-2a4c-46ae-af2d-684d00807d69', print_economics_determine_religious_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('882cb707-2a4c-46ae-af2d-684d00807d69', secondary, reformers_are_downstream_adapters).
narrative_ontology:cs_axiom_status(reformers_are_downstream_adapters, holdable).
narrative_ontology:cs_axiom_grounding('882cb707-2a4c-46ae-af2d-684d00807d69', reformers_are_downstream_adapters, empirically_contingent).
narrative_ontology:cs_reference_frame('882cb707-2a4c-46ae-af2d-684d00807d69', print_as_causal_substrate).
narrative_ontology:cs_drift_state('882cb707-2a4c-46ae-af2d-684d00807d69', contemporary_post_agency_turn, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('882cb707-2a4c-46ae-af2d-684d00807d69', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_lay_readers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, territorial_princes).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, scribal_workshops).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, ecclesiastical_gatekeepers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, media_determinism_research_program).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invest capital in presses, type, and paper supply, and decide which works get multiplied and in which languages. Edition economics reward large runs of short vernacular texts; shops serve whichever buyers pay, moving between devotional, classical, and controversial titles without doctrinal attachment. Leaving the trade means liquidating specialized equipment, but the skills transfer across genres and markets.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, commercial_printers, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, commercial_printers, agenda_setter).

% Buy scripture portions, pamphlets, and catechisms at prices a skilled household can afford for the first time. Before cheap print, access to written religion ran through clergy and Latin literacy. They have no say in what gets printed and no organization representing their preferences.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_lay_readers, beneficiary,
    powerless, biographical, constrained, continental).

% Compose translations, tracts, and sermons sized to the new edition formats and rely on merchant book networks to carry them across borders. Public commitment brings excommunication and imperial ban; stepping back means repudiating their own teaching and facing heresy proceedings, so the commitment, once made, has no return path.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, protestant_reformers, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__technological_determinism_reading, protestant_reformers, agenda_setter).

% Copy manuscripts for a living; within two generations of print diffusion their prices cannot compete for ordinary books. Some shift to luxury volumes, legal engrossing, and secretarial work; guild protections cannot restore cost parity. Their skill is specific to the displaced method, and retraining means abandoning the craft that organized their working life.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, scribal_workshops, payer,
    moderate, biographical, trapped, regional).

% Control which texts circulate through scriptoria, licensure, and Latin-language formation, collecting deference and fees from that mediation. Cheap print dissolves the scarcity their position rests on. They respond with index lists, vernacular restrictions, and internal disciplinary reform, while simultaneously adopting print for their own approved output; abandoning doctrine is not an option they hold.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, ecclesiastical_gatekeepers, payer,
    institutional, generational, constrained, continental).

% Use printed ordinances and administrative instruments to govern at larger scale than chancery copying allowed. Where religious realignment occurs, printed controversy supplies justification for asserting jurisdiction over church property and courts. Dynastic and confessional commitments bind them to whatever alignment they adopt.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, territorial_princes, beneficiary,
    powerful, generational, constrained, national).

% Modern scholars comparing regions and periods where identical print access produced different religious outcomes. They hold the comparative record that tests causal claims about the press, and they publish the critiques and defenses that define the current dispute over how much the technology determined.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__technological_determinism_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_reformation_causality__technological_determinism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the text-reproduction problem: once a work is composed, movable-type economics multiply copies at a fraction of manuscript cost and move them through merchant networks across linguistic boundaries, coordinating access to written knowledge at population scale.
% TRANSFER_FUNCTION: Moves textual access from a scarce, clergy-mediated commodity to a mass commodity; correspondingly moves distribution rents from scriptoria and licensed gatekeepers to printers and to readers as price surplus, and moves doctrinal initiative from central authorities to whoever can fund a press run.
% ABSENT_VOICES: Scribal households whose incomes the transition destroyed left almost no record — they lacked both the literacy infrastructure and a print voice, so the losers' testimony is structurally missing from the archive. Conciliarist theologians who predicted doctrinal fragmentation wrote against the trend and were marginalized. Ordinary readers' reception survives mainly through what authorities condemned, not what they thought.
% DISAPPEARANCE_RATIONALE: If the cost structure reverted overnight — if multiplying a text again cost a scribe months of labor — vernacular scripture would remain a rare, patron-sponsored luxury, reform programs would stay clerical and court-bound, and the confessional map of Europe would not fragment as it did. The pamphlet war, the catechism economy, and the bible-translation wave all depend on the price collapse.
% FOUNDING_PROBLEM: The cost ceiling on reproducing texts: manuscript copying capped circulation at elite scale and made any mass reading public impossible; the press arrangement was built to break that ceiling.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book (the Febvre–Martin tradition) and surviving price records corroborate the cost collapse from outside any confessional or determinist camp; scribal guild decay records and scriptorium closures corroborate independently. By contrast, no source outside the determinist tradition attests the further inference that inevitability followed from the cost collapse — agency-school historians explicitly contest that step. Corroboration therefore covers the substrate, not this reading's causal conclusion.
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18) because the operative force of this constraint, as the reading itself specifies, is a cost collapse — a generative subsidy to access, not a rent-collection design. Residual extraction concentrates in the transition decades (series peaks 0.23 around 1510) as scribal livelihoods and gatekeeping positions are destroyed faster than their holders can adapt, then decays as the displacement completes; the end-state scalar matches the 1555 measurement. Suppression is low (0.18) and UNSCALED by construction — it records the raw coercive deficit of the structure: print economics compel no one and forbid nothing; the Church's countermeasures are enforcement AGAINST the constraint's outputs, not maintenance OF the constraint, so no suppression_requirement series is authored (static enforcement picture; the scalar carries it). Theater is near-zero (0.05): a physical cost structure is not performed. Accessibility collapse is high (0.82): manuscript production collapsed commercially within two generations, with residual liturgical and artistic niches keeping it below full natural-law closure. Resistance (0.45) is the honest tell against mountain certification: index lists, licensing, and vernacular bans constitute sustained organized opposition that physics does not attract. Both tracked series run on ONE shared seven-point grid (1450–1555) so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the scribal and gatekeeping seats, the same price curve that subsidizes readers reads as dispossession — their per-seat computations should sit far nearer the target end than the reader seat's, despite the low base epsilon. Two same-level organized actors diverge on EXIT rather than power: printers hold arbitrage (they printed for Rome and Wittenberg alike, following demand) while reformers hold identity-locked commitment (public recantation meant ruin), so identical nominal standing produces opposite directionalities. The observer seat sees the classification contest itself: the inevitability premise is exactly what the comparative record — print-rich Catholic Italy, Spain, France; print-banning Ottoman realms — puts under test.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: readers receive price surplus (d near the beneficiary end), printers receive margins plus agenda control with arbitrage damping any exposure further, reformers receive reach (d low despite identity lock — the lock binds them to a channel they benefit from, not one that extracts from them), princes receive administrative and jurisdictional capability. Victim declarations: scribes bear income destruction with trapped exit amplifying their exposure (d near the target end); gatekeepers bear monopoly dissolution — institutional power does not shield them because the asset being devalued IS their position. No directionality overrides were needed: declaration-plus-exit derivation already separates every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The genealogy interview returns a mismatch: the founding problem (the manual-reproduction cost ceiling) is dead, while the arrangement persists and still rearranges the world (world_rearranges). The theater series arbitrates the zombie risk that mismatch flags: at 0.02–0.05 across the whole interval, the arrangement's persistence is functional delivery, not performed ritual, resolving toward living infrastructure rather than mandate-outlived-function. The same analysis protects against the opposite mislabeling: the extraction series peaks during the transition and decays, consistent with one-time creative destruction rather than an ongoing extraction design — reading the displacement costs as proof of hidden rent-collection would convert a rope-shaped subsidy into a phantom snare. Finally, the mountain claim itself is routed through false-summit review because beneficiaries are declared; the natural-law-versus-constructed omega is where that adjudication lives, and the dead-founding-problem finding feeds it directly: arrangements routinely dress solved problems in inevitability language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which reading of the technology_reformation_causality kernel does this story instantiate, and what would the sibling readings change structurally?',
    'Cross-reading comparison: recompute beneficiary/victim declarations and epsilon under beneficiary_agency_reading (causation relocated into strategic deployment) and co_constitution_reading (causation distributed across mutual shaping); divergent classifications across the three files locate the disagreement precisely.',
    'Under beneficiary_agency_reading the mountain claim dissolves into a tool-account with strategic, targeted beneficiaries and moderate epsilon; under co_constitution_reading neither pure technological necessity nor pure agency survives and classification becomes least settled. This file''s mountain claim holds only within the determinist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is the technological_determinism_reading of the print-Reformation kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    natural_law_vs_constructed_inevitability,
    'Is print-driven Reformation a genuine structural necessity of information economics, or a constructed inevitability narrative serving identifiable interpretive interests?',
    'Comparative counterfactual analysis: jurisdictions with identical print access that remained Catholic (Italy, Spain, France) and polities that suppressed print without reformation pressure (Ottoman realms); if print-compatible religious stability is robust across cases, the necessity premise fails.',
    'If the counterexamples hold, the mountain certification fails and the constraint recomputes along the false-summit path as a contested discursive construct; if print access reliably predicts rupture, the mountain claim strengthens and the beneficiary declarations read as incidental rather than constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_inevitability, empirical, 'Required natural-law versus constructed ambiguity documentation for a mountain claiming beneficiaries.').

omega_variable(
    displacement_cost_accounting,
    'Do the transition''s destruction of scribal livelihoods and the uncompensated dissolution of ecclesiastical gatekeeping count as extraction performed by the constraint, or as ordinary creative destruction external to it?',
    'Boundary analysis against analogous skill-displacement episodes: extraction attribution turns on whether the cost curve targeted incumbents or was indifferent to them — indifference favors externalizing the losses, targeting favors internalizing them into epsilon.',
    'Counting displacement as extraction raises epsilon above 0.3 and pulls classification away from mountain purity toward tangled_rope; excluding it preserves the low-extraction mountain profile this reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_cost_accounting, conceptual, 'Whether transition losses belong inside or outside the constraint''s extraction ledger.').

omega_variable(
    incumbent_cooption_weight,
    'How much does the incumbent Church''s own decades-long adoption of print — printed indulgences, standardized breviaries, defensive polemic — weigh against the dissolution premise?',
    'Quantify incumbent print output share and institutional benefit from 1450 to 1517; if the old regime was a net print beneficiary up to the reform moment, the mechanism behind inevitability weakens.',
    'Strong incumbent co-option supports the agency and co-constitution siblings and undermines this reading''s reference frame; weak co-option leaves the cost-collapse mechanism intact and the mountain claim standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_cooption_weight, empirical, 'Incumbent adoption of the technology as counter-evidence to structural doom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tref_techdet_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.02).
narrative_ontology:measurement_basis(tref_techdet_tr_t1450, observed).
narrative_ontology:measurement(tref_techdet_tr_t1470, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1470, 0.03).
narrative_ontology:measurement_basis(tref_techdet_tr_t1470, observed).
narrative_ontology:measurement(tref_techdet_tr_t1490, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1490, 0.03).
narrative_ontology:measurement_basis(tref_techdet_tr_t1490, observed).
narrative_ontology:measurement(tref_techdet_tr_t1510, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1510, 0.04).
narrative_ontology:measurement_basis(tref_techdet_tr_t1510, observed).
narrative_ontology:measurement(tref_techdet_tr_t1525, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1525, 0.04).
narrative_ontology:measurement_basis(tref_techdet_tr_t1525, observed).
narrative_ontology:measurement(tref_techdet_tr_t1540, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1540, 0.05).
narrative_ontology:measurement_basis(tref_techdet_tr_t1540, observed).
narrative_ontology:measurement(tref_techdet_tr_t1555, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1555, 0.05).
narrative_ontology:measurement_basis(tref_techdet_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(tref_techdet_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.07).
narrative_ontology:measurement_basis(tref_techdet_be_t1450, observed).
narrative_ontology:measurement(tref_techdet_be_t1470, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1470, 0.13).
narrative_ontology:measurement_basis(tref_techdet_be_t1470, observed).
narrative_ontology:measurement(tref_techdet_be_t1490, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1490, 0.19).
narrative_ontology:measurement_basis(tref_techdet_be_t1490, observed).
narrative_ontology:measurement(tref_techdet_be_t1510, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1510, 0.23).
narrative_ontology:measurement_basis(tref_techdet_be_t1510, observed).
narrative_ontology:measurement(tref_techdet_be_t1525, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1525, 0.21).
narrative_ontology:measurement_basis(tref_techdet_be_t1525, observed).
narrative_ontology:measurement(tref_techdet_be_t1540, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1540, 0.19).
narrative_ontology:measurement_basis(tref_techdet_be_t1540, observed).
narrative_ontology:measurement(tref_techdet_be_t1555, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1555, 0.18).
narrative_ontology:measurement_basis(tref_techdet_be_t1555, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__technological_determinism_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'printing caused the Reformation' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file instantiates the technological_determinism_reading: technology as fixed substrate, epsilon located in the production-cost collapse, low attributable extraction, mountain claim. technology_reformation_causality__beneficiary_agency_reading locates causation in strategic deployment by reformers and printers — epsilon relocates into targeted strategy (moderate), beneficiaries become choosing agents rather than downstream adapters. technology_reformation_causality__co_constitution_reading distributes causation across mutual shaping of press and movement — attributable extraction is lowest and classification least settled. Each file carries its own beneficiaries, victims, epsilon, and claimed type; the upstream/downstream evidentiary gradient runs from the corroborated cost-collapse substrate (shared by all three) to the contested inevitability inference (unique to this reading). Edges here express family membership, not endorsement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
