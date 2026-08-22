% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Electronic Money as Conceptual-Diffusion Emergence (Became-Thinkable Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the 'became thinkable' reading of the electronic
 *   money emergence kernel: the claim that digital/electronic money emerged
 *   as a diffuse, decades-long process of conceptual and social normalization
 *   — telegraphic transfers, interbank giro systems, punch-card ledger
 *   banking, and early computing making non-physical value transfer
 *   technically feasible and socially acceptable — well before any central
 *   bank or statistical office began measuring 'electronic money' as a
 *   discrete monetary aggregate. Under this reading there is no single
 *   threshold event; emergence is gradual diffusion, and institutional
 *   measurement (the M4/M5-style aggregates) is a belated administrative
 *   catch-up rather than the origin point. This is one of three readings of
 *   the same kernel. The first_held_reading instead locates emergence at a
 *   discrete institutional moment (first dematerialized instrument held). The
 *   m4_m5_collapse_reading denies real emergence altogether, treating the
 *   entire category as retroactively created by statistical convention. These
 *   are not measurement variants of one constraint — they are three
 *   structurally distinct claims about causal priority, agency, and what
 *   counts as 'real' emergence, each with its own beneficiary structure and
 *   its own epsilon.
 *
 * KEY AGENTS:
 *   - early_computing_and_telecoms_engineers: technical innovators credited as originary agents under this reading
 *   - clearing_and_settlement_innovators: institutional-social innovators who normalized ledger money
 *   - monetary_theorists_of_dematerialization: theorists whose diffusion account is vindicated
 *   - central_bank_statistical_offices: cast as belated measurers rather than originating authorities
 *   - economic_historians: analytical observers adjudicating between readings
 *   - pre_digital_cash_dependent_populations: structurally excluded from the entire contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.28).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Electronic Money as Conceptual-Diffusion Emergence (Became-Thinkable Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'd23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a').
narrative_ontology:cs_kernel_codification('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', distributed).
narrative_ontology:cs_authority_grounding('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', distributed).
narrative_ontology:cs_reading_relation('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', foundational, conceptual_possibility_precedes_institutional_recognition).
narrative_ontology:cs_axiom_status(conceptual_possibility_precedes_institutional_recognition, holdable).
narrative_ontology:cs_axiom_grounding('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', conceptual_possibility_precedes_institutional_recognition, empirically_contingent).
narrative_ontology:cs_axiom('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', foundational, emergence_is_gradual_diffusion_not_discrete_event).
narrative_ontology:cs_axiom_status(emergence_is_gradual_diffusion_not_discrete_event, holdable).
narrative_ontology:cs_axiom_grounding('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', emergence_is_gradual_diffusion_not_discrete_event, empirically_contingent).
narrative_ontology:cs_reference_frame('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', diffusion_based_technical_social_thinkability).
narrative_ontology:cs_drift_state('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', post_ecommerce_digital_currency_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d23a75dc-dc5d-4a98-bc8e-2ff6dcc3cd6a', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, early_computing_and_telecoms_engineers).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, clearing_and_settlement_innovators).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, central_bank_statistical_offices).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, conceptual_possibility_precedes_institutional_recognition).
narrative_ontology:constraint_vindicates(electronic_money_emergence__became_thinkable_reading, diffusion_model_of_monetary_innovation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built the batch-processing, telegraphic transfer, and later networked ledger systems that made non-physical value transfer technically conceivable. They gain intellectual and professional credit under this reading because it locates the origin of electronic money in the moment their systems made dematerialized value thinkable, well before any regulator counted it as a monetary aggregate.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_computing_and_telecoms_engineers, beneficiary,
    moderate, generational, mobile, global).

% Designed interbank clearing arrangements, giro systems, and correspondent-banking telegraphic transfers that socially normalized ledger-based money movement. Under this reading they are credited as the site of emergence, since social thinkability required both a technical substrate and institutional actors willing to treat entries as money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, clearing_and_settlement_innovators, beneficiary,
    organized, generational, mobile, continental).

% Academic and policy economists who argue money's essence was always relational/informational rather than material. This reading vindicates their long-standing claim that the conceptual shift, not the artifact or the statistic, is the real historical event — they gain intellectual standing without material extraction.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization, beneficiary,
    moderate, civilizational, analytical, global).

% Responsible for defining and measuring monetary aggregates (M1 through M5-type categories). This reading treats their measurement apparatus as structurally belated — a downstream administrative catch-up to a phenomenon that had already emerged decades earlier in engineering and banking practice. They bear the interpretive cost of being cast as perpetually lagging rather than as the authority that defines what counts as money in the first place.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_bank_statistical_offices, payer,
    institutional, generational, constrained, national).

% Study the archival record of telegraphic transfers, punch-card ledgers, giro networks, and early wire systems to adjudicate when 'thinkability' actually crystallized. They weigh this reading against the first-held and measurement-artifact readings using primary sources rather than institutional statistics.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% Populations without access to banking, telegraphy, or computing infrastructure during the diffusion period had no stake in when 'electronic money' is dated to have emerged and are never consulted in any of the three readings; the entire kernel contest occurs among institutional and technical elites.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, pre_digital_cash_dependent_populations, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Locating emergence in the moment of conceptual/technical thinkability coordinates historical and economic explanation around causal priority: it lets engineers, bankers, and theorists identify the actual innovation event rather than an administrative artifact, which matters for understanding how monetary innovation diffuses and what enables it.
% TRANSFER_FUNCTION: Moves interpretive credit and historical priority from institutional record-keepers (statistical offices, central banks) toward technologists, clearing-system designers, and theorists who first made dematerialized value practically and socially conceivable. No material resource transfer occurs; what moves is explanatory authority and narrative primacy.
% ABSENT_VOICES: Populations excluded from banking and computing infrastructure during the relevant decades have no voice in dating the emergence and are structurally irrelevant to all three readings, since the contest concerns institutional and technical actors' conceptual and administrative timelines, not lived monetary experience.
% DISAPPEARANCE_RATIONALE: If this reading vanished, economic historiography would default toward either the first-held reading (a discrete institutional threshold) or the measurement-artifact reading (denying any real emergence event). Technologists and clearing-system designers would lose their claim to originary credit, and monetary theory would lose a diffusion-based account of innovation causality — but no material institution or transfer would be disrupted, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Historical and economic-theoretic dissatisfaction with dating monetary innovation solely by when regulators began counting it — a felt need to locate the actual causal moment when non-physical value transfer became conceivable and practiced, rather than when it was administratively recognized.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by economic historians outside the three interested reading-communities (e.g., historians of computing and payments infrastructure who have no stake in central-bank statistical categories or in claiming engineering priority); their archival work on telegraphic transfer and giro systems is cited by multiple contesting camps, which is itself evidence the founding problem is recognized outside the benefiting parties, though its resolution (which reading is correct) remains actively disputed.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because this reading describes an intellectual/historiographical priority claim, not a material extraction mechanism — the main 'cost' borne by statistical offices is reputational/interpretive (being framed as lagging), not financial. Suppression is low (0.22): no one is coerced into accepting this reading, and rival readings circulate freely in the historiographical literature. Theater ratio rises over the interval (0.15 to 0.40) because as institutional statistical apparatus matured, the appearance of precise measurement (M1-M5 categories) increasingly performed authoritative dating of monetary phenomena while the actual conceptual/technical diffusion this reading tracks had already occurred decades prior — the theater is the increasing gap between administrative certainty and messy historical diffusion. Accessibility collapse is moderate (0.35): once the diffusion account is understood, the discrete-threshold framing becomes less persuasive, but does not vanish, since institutional actors have strong incentives to retain threshold-based measurement for policy purposes.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineers, clearing-system designers, and dematerialization theorists are coded as beneficiaries because this reading assigns them causal and historical priority — a low-cost, high-credit position with mobile or analytical exit options and no structural dependency on any single institution's cooperation. Central bank statistical offices are coded as payers not because they lose money but because the reading structurally subordinates their authority to define 'emergence' to a prior, uncounted diffusion process outside their instruments — their exit is constrained because their institutional mandate requires them to keep measuring regardless of whether the reading undermines their claim to originary authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (locating the real causal moment of monetary innovation rather than deferring to administrative dating) remains live: historians and technologists continue to find this an open and productive question, corroborated by archival work outside the three contesting camps. There is no mandatrophy here in the classic sense — no institution's original coordination function has died while extraction persists — because this is a historiographical/interpretive contest, not an enforced institutional arrangement. The main risk of mislabeling would be treating the low extraction/low suppression profile as meaning the reading is 'true' or 'neutral'; it is simply less extractive as a social arrangement than a kernel reading that concentrated rents or coercive enforcement around a single dating convention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_dating_indeterminacy,
    'Is there a principled way to date when a conceptual/technical possibility became ''socially thinkable'' as opposed to merely technically feasible, or is this threshold itself constructed retrospectively by historians selecting convenient milestones (telegraphic transfer, punch-card banking, early EFT)?',
    'Comparative archival analysis of contemporaneous discourse (banking journals, engineering trade publications, regulatory correspondence) across candidate diffusion milestones to see whether ''thinkability'' shows a genuine inflection or is a smooth continuous gradient with no defensible threshold.',
    'If no principled threshold exists, this reading''s claim to identify a determinate (if gradual) emergence process collapses toward the m4_m5_collapse_reading''s position that all dating is retrospective construction; if a genuine inflection is found, it strengthens this reading''s claim over both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkability_dating_indeterminacy, conceptual, 'Whether ''became thinkable'' names a real historical process or a retrospectively selected narrative convenience.').

omega_variable(
    kernel_framing_choice_between_readings,
    'Given that the kernel (electronic money emergence) admits at least three structurally distinct readings (thinkability-diffusion, first-institutional-holding, measurement-artifact), what determined the choice to author this story as the thinkability reading rather than defaulting to the more common first-held or the more deflationary m4_m5_collapse framing?',
    'Document the selection rationale: the SCOPE manifest explicitly assigned this reading; the alternative framings are authored as separate sibling constraints per the eps-invariance decomposition rule rather than folded into this one story.',
    'Had the m4_m5_collapse framing been chosen instead, extractiveness and beneficiary structure would look entirely different (statistical offices would be beneficiaries of a category they created rather than payers subordinated to a prior process) — confirming these are genuinely different constraints, not measurement variants of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice_between_readings, conceptual, 'Documents the committer-frame choice among three kernel readings and what would change under a sibling framing.').

omega_variable(
    engineer_credit_beneficiary_status,
    'Do early computing and telecoms engineers actually derive any material benefit from being credited as originators of electronic money, or is ''beneficiary'' status here purely symbolic/historiographical with no real-world consequence?',
    'Survey whether historical priority claims in monetary innovation historiography translate into any measurable professional, institutional, or funding advantage for the credited fields (e.g., computing history departments, fintech origin narratives used in industry marketing).',
    'If purely symbolic, the beneficiary designation should be understood as very low-intensity (near-zero real extraction), reinforcing the low base extractiveness score; if it feeds into real fintech-industry origin myths with commercial value, the beneficiary relationship is stronger than currently modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(engineer_credit_beneficiary_status, empirical, 'Whether originary credit under this reading carries any real material benefit for the credited technical community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1870, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1870, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement_basis(elec_tr_t1870, observed).
narrative_ontology:measurement(elec_tr_t1900, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement_basis(elec_tr_t1900, observed).
narrative_ontology:measurement(elec_tr_t1930, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1930, 0.28).
narrative_ontology:measurement_basis(elec_tr_t1930, observed).
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1950, 0.33).
narrative_ontology:measurement_basis(elec_tr_t1950, observed).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.37).
narrative_ontology:measurement_basis(elec_tr_t1970, observed).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement_basis(elec_tr_t1990, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t1870, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1870, 0.1).
narrative_ontology:measurement_basis(elec_be_t1870, observed).
narrative_ontology:measurement(elec_be_t1900, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1900, 0.14).
narrative_ontology:measurement_basis(elec_be_t1900, observed).
narrative_ontology:measurement(elec_be_t1930, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1930, 0.18).
narrative_ontology:measurement_basis(elec_be_t1930, observed).
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(elec_be_t1950, observed).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement_basis(elec_be_t1970, observed).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement_basis(elec_be_t1990, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'electronic money emergence' per the eps-invariance principle. became_thinkable_reading (this story) authors low extraction/low suppression consistent with a diffuse historiographical priority contest. first_held_reading authors emergence as a discrete institutional threshold event with different beneficiary/victim structure. m4_m5_collapse_reading treats the entire emergence narrative as a statistical-measurement artifact, denying that any real emergence event occurred independent of the M4/M5 category construction — this is the most epistemically deflationary reading and would show a very different extraction profile (likely extraction borne by anyone who took the 'emergence' narrative as literal history). All three share the kernel_id electronic_money_emergence and are linked bidirectionally via network.affects_constraints; each carries its own epsilon and must not be averaged or reconciled with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
