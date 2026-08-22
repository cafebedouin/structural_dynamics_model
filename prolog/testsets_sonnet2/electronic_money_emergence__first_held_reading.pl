% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding of Dematerialized Currency as the Emergence Event
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the 'first-held' reading of the
 *   electronic-money-emergence kernel: the claim that digital money came into
 *   being at a discrete, identifiable institutional moment — when some bearer
 *   institution first held dematerialized currency in a form legally and
 *   operationally distinguishable from physical notes. This reading treats
 *   emergence as an event with a date, a location, and an institutional
 *   author, analogous to a legal recognition threshold. It stands in explicit
 *   contrast to two sibling readings (generated as separate constraint
 *   stories): the 'became thinkable' reading, which locates emergence in a
 *   pre-institutional conceptual/technical possibility space, and the 'M4/M5
 *   collapse' reading, which denies a real emergence event altogether and
 *   treats the appearance of one as an artifact of statistical categorization
 *   (the M4/M5 monetary aggregate boundary). All three readings describe the
 *   same historical kernel — the transition from physical to dematerialized
 *   currency — but assign the ontological weight of 'emergence' to
 *   structurally different loci: an institutional custody event (this story),
 *   a conceptual/technical threshold (sibling 1), or a measurement convention
 *   (sibling 2). Because each locus implies a different beneficiary structure
 *   (whoever controls the recognized threshold gains narrative and regulatory
 *   priority), each reading is authored as its own constraint with its own
 *   epsilon, per the ε-invariance principle — they are not the same
 *   constraint viewed three ways.
 *
 * KEY AGENTS:
 *   - settlement_banks_holding_first_dematerialized_balances: primary beneficiary (institutional/arbitrage) — gains historiographical and regulatory priority from being named 'first'
 *   - central_bank_ledger_administrators: agenda-setter (institutional/analytical) — certifies the recognition threshold that operationalizes this reading
 *   - cash_dependent_depositors: payer (powerless/trapped) — bears downstream regulatory recategorization with no participation in defining the threshold
 *   - unbanked_and_underbanked_populations: payer (powerless/trapped) — structurally excluded from the event this reading privileges
 *   - monetary_historians_and_regulators: analytical observer — adjudicates between competing emergence narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.31).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding of Dematerialized Currency as the Emergence Event").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'a68cf5ae-e1a6-40c3-9f36-ba470f35f88e').
narrative_ontology:cs_kernel_codification('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', distributed).
narrative_ontology:cs_authority_grounding('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', lineage).
narrative_ontology:cs_interpretation_layer_present('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e').
narrative_ontology:cs_reading_relation('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', foundational, institutional_custody_constitutes_ontological_transition).
narrative_ontology:cs_axiom_status(institutional_custody_constitutes_ontological_transition, holdable).
narrative_ontology:cs_axiom_grounding('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', institutional_custody_constitutes_ontological_transition, conventional).
narrative_ontology:cs_axiom('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', secondary, legal_recognition_threshold_tracks_real_emergence).
narrative_ontology:cs_axiom_status(legal_recognition_threshold_tracks_real_emergence, holdable).
narrative_ontology:cs_axiom_grounding('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', legal_recognition_threshold_tracks_real_emergence, instrumental).
narrative_ontology:cs_reference_frame('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', discrete_institutional_recognition_threshold).
narrative_ontology:cs_drift_state('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', contemporary_cbdc_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a68cf5ae-e1a6-40c3-9f36-ba470f35f88e', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, settlement_banks_holding_first_dematerialized_balances).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_bank_ledger_administrators).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, cash_dependent_depositors).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, unbanked_and_underbanked_populations).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, smaller_correspondent_banks_outside_first_settlement_tier).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, discrete_event_theory_of_monetary_form_change).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_custody_as_ontological_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The clearing/settlement institutions that were first to hold and record book-entry balances distinguishable from physical notes get to define, by their own operational practice, when 'digital money' began. Their internal ledgers become the historical record cited by regulators and historians alike. They benefit from being named the origin point: it consolidates their claim to have pioneered the payments infrastructure that later commercial banks had to interconnect with on terms the first-movers set.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, settlement_banks_holding_first_dematerialized_balances, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, settlement_banks_holding_first_dematerialized_balances, agenda_setter).

% Central banks certify which balances count as legally recognized money and administer the regulatory recognition threshold this reading treats as the emergence event. They set the criteria (reserve accounts, settlement finality rules) that determine which institution's holding counts as 'first.' Their authority to declare the threshold is itself part of what the constraint distributes.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_bank_ledger_administrators, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, central_bank_ledger_administrators, beneficiary).

% Ordinary depositors whose money existed as physical notes before, during, and after the institutional threshold event experienced no discrete change in their own economic reality on the date the first bearer institution held dematerialized currency. Yet the legal/regulatory recognition event reshapes the rules governing their deposits (fractional reserve treatment, deposit insurance categorization, seizure/freeze mechanics) without their participation. They cannot opt out of a monetary system whose ontology was redefined institutionally.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, cash_dependent_depositors, payer,
    powerless, biographical, trapped, national).

% Populations without accounts at the institutions that held the first dematerialized balances are structurally outside the event this reading privileges as 'emergence.' Their monetary experience continued unchanged in physical cash while the recognized origin story of digital money was written entirely inside institutions they had no access to. The recognition threshold's institutional bias systematically erases their parallel (non-)transition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, unbanked_and_underbanked_populations, payer,
    powerless, generational, trapped, national).

% Smaller banks that adopted dematerialized book-entry practices later than the first-tier settlement banks are relegated to 'follower' status in the historical and regulatory record. This affects their standing in disputes over settlement priority, capital treatment transition rules, and access to central bank facilities, since regulatory frameworks often grandfather rights to the institutions recognized as having been first.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, smaller_correspondent_banks_outside_first_settlement_tier, payer,
    moderate, generational, constrained, national).

% Scholars and regulators who adjudicate competing emergence narratives (first-held vs. became-thinkable vs. statistical-artifact readings) and whose choice of narrative has downstream consequences for how monetary aggregates, legal tender status, and central bank digital currency debates are framed today.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians_and_regulators, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, settlement_banks_holding_first_dematerialized_balances).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixing a discrete institutional threshold — the first bearer holding dematerialized currency in a form distinguishable from notes — lets regulators, courts, and historians coordinate on a single, legally administrable moment for 'digital money exists' rather than litigating a fuzzy, continuous socio-technical process case by case.
% TRANSFER_FUNCTION: The reading transfers narrative and regulatory priority from populations and institutions whose monetary experience changed gradually or not at all, to the specific settlement institutions and central bank administrators whose internal ledger event gets canonized as the origin point — with downstream effects on grandfathered settlement rights, legal-tender interpretation, and historical credit for innovation.
% ABSENT_VOICES: Unbanked populations and smaller correspondent banks have no seat in defining when 'digital money' began, despite the definition affecting the regulatory categories later applied to their money and their institutions. Non-Western monetary systems that dematerialized currency through different institutional pathways (e.g., non-Western clearing houses) are also absent from a reading anchored to Western settlement-bank recordkeeping.
% DISAPPEARANCE_RATIONALE: If the first-held-reading's institutional threshold were abandoned as the definition of emergence, legal and regulatory categories keyed to a discrete origin date (deposit insurance treatment, settlement finality statutes referencing 'from the date electronic money was first recognized') would need re-derivation from a different theory of monetary ontology — a genuine rearrangement for regulators and litigants. But for the underlying economic reality of payments and deposits, nothing would change: the reading is a historiographical and legal convention layered onto processes that continued regardless of which threshold got canonized. Parties dispute which effect dominates.
% FOUNDING_PROBLEM: Legal and regulatory systems needed a bright-line, adjudicable moment to anchor statutes, deposit insurance schemes, and settlement-finality law that presuppose money either is or is not 'electronic' — continuous socio-technical change is administratively unworkable for law, which needs discrete triggers.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the settlement-bank tradition (comparative payments-law literature) corroborate that discrete-threshold conventions remain operationally necessary for statutes referencing electronic money's legal status, independent of whether the settlement banks' own claimed priority is historically accurate. Historians of technology (outside the banking sector) dispute whether the institutional-holding moment is the correct anchor rather than the thinkability or statistical-classification moments, but agree some administrable trigger is functionally required.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.31 at interval end) rather than high, because the primary transfer is narrative/regulatory priority rather than direct material extraction — the settlement banks and central bank administrators gain standing and grandfathered treatment, not ongoing rents extracted from a captive population. Suppression is low-moderate (0.22) and essentially flat across the interval: there is no active mechanism forcing populations to accept the first-held threshold as authoritative beyond the ordinary weight of legal and regulatory convention, and that weight has not meaningfully intensified over sixty years. Theater ratio is modest (0.18) and rises slowly, reflecting some performative reinforcement of the 'discrete origin' narrative in institutional histories and central bank commemorations, but a real underlying function: legal systems genuinely need an administrable trigger date, which is not merely theater. Accessibility collapse is authored moderately high (0.6): once the institutional-threshold framing is adopted by regulators and courts, the alternative framings (thinkability, statistical artifact) become progressively harder to argue in a legal or policy context, though they remain live in historical and economic scholarship — hence collapse is real but not near-total.
 *
 * DIRECTIONALITY LOGIC:
 *   Settlement banks and central bank administrators are structural beneficiaries: they set and benefit from the criteria that determine who counts as 'first,' consolidating historiographical priority and often receiving grandfathered regulatory treatment tied to being recognized as the pioneering holder. Cash-dependent depositors and unbanked populations are targets: the threshold event reshapes the regulatory categories applied to their money (deposit insurance treatment, legal tender status debates) without their participation, and unbanked populations are further harmed by being definitionally excluded from the event that supposedly marks monetary modernity. Smaller correspondent banks sit at moderate power with constrained exit — they can eventually adopt dematerialized practices themselves but cannot retroactively claim priority, which affects their standing in settlement-rights disputes.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification captures a genuine coordination function (legal and regulatory systems need a discrete, administrable trigger for statutes that presuppose electronic money either exists or doesn't) coexisting with asymmetric extraction (the specific institutions credited with 'first' status gain narrative and regulatory priority that smaller and excluded actors do not share). This prevents two mislabeling errors: treating the threshold purely as neutral historical fact (ignoring that its administrative existence serves and was partly shaped by the institutions it credits) and treating it purely as extraction (ignoring the genuine legal-administrability problem the threshold solves). The founding problem — law's need for a bright-line trigger — remains live, which is why this is tangled rather than a pure snare or a piton: the coordination function has not atrophied, it coexists with the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_threshold_vs_conceptual_priority,
    'Is the correct locus of ''emergence'' the discrete institutional custody event this reading privileges, or does genuine emergence properly belong to the earlier moment when dematerialized currency became conceptually and technically thinkable (the sibling reading)?',
    'Historical and philosophy-of-technology analysis of whether legal/regulatory recognition tracks or merely ratifies a prior technical possibility; comparative study of other technology emergence debates (e.g., invention vs. patent-filing vs. commercial deployment as the ''true'' origin).',
    'If conceptual priority is the more defensible locus, this reading''s beneficiary structure (settlement banks, central bank administrators) is exposed as having captured credit for an event that was already substantively determined earlier by different actors (engineers, theorists) who receive no comparable recognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_threshold_vs_conceptual_priority, conceptual, 'Whether institutional custody or prior technical thinkability is the more defensible emergence locus.').

omega_variable(
    measurement_artifact_vs_real_event,
    'Is there a real ontological transition being tracked here at all, or does the appearance of a discrete ''emergence'' derive from the M4/M5 statistical classification boundary retroactively imposing a threshold narrative onto a continuous process (the third sibling reading)?',
    'Examine whether independent evidence (contemporaneous institutional records, court cases, contracts) treated the threshold as significant BEFORE the statistical aggregate distinction was formalized, versus whether the ''first held'' narrative was constructed after the fact to match aggregate reporting categories.',
    'If the emergence narrative postdates and was shaped by the statistical classification, this reading''s claimed institutional beneficiaries would need to be re-examined for whether they benefited from a real priority or from being retroactively slotted into a categorization scheme that served central bank reporting convenience.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_artifact_vs_real_event, empirical, 'Whether the discrete emergence event is a real institutional fact or a retroactive statistical construction.').

omega_variable(
    natural_law_vs_constructed_threshold,
    'Because this reading treats the emergence threshold as an observable, near-objective fact akin to natural law (a specific institution held dematerialized currency at a specific date), is this framing itself serving the interests of the institutions credited as ''first,'' independent of whether a more contestable framing would be equally defensible?',
    'Comparative institutional history: check whether the specific settlement banks/central banks credited with priority actively lobbied, funded, or shaped the historiography establishing their own priority claim.',
    'If the credited institutions shaped their own priority narrative, the apparent ''discrete event'' character of this reading is itself a constructed artifact serving identifiable beneficiaries, strengthening the case for tangled_rope over a more benign rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_threshold, empirical, 'Whether the discreteness of the institutional threshold is itself a construction serving the credited institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(elec_tr_t0, observed).
narrative_ontology:measurement(elec_tr_t12, electronic_money_emergence__first_held_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(elec_tr_t12, observed).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(elec_tr_t24, observed).
narrative_ontology:measurement(elec_tr_t36, electronic_money_emergence__first_held_reading, theater_ratio, 36, 0.15).
narrative_ontology:measurement_basis(elec_tr_t36, observed).
narrative_ontology:measurement(elec_tr_t48, electronic_money_emergence__first_held_reading, theater_ratio, 48, 0.17).
narrative_ontology:measurement_basis(elec_tr_t48, observed).
narrative_ontology:measurement(elec_tr_t60, electronic_money_emergence__first_held_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(elec_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(elec_be_t0, observed).
narrative_ontology:measurement(elec_be_t12, electronic_money_emergence__first_held_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(elec_be_t12, observed).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement_basis(elec_be_t24, observed).
narrative_ontology:measurement(elec_be_t36, electronic_money_emergence__first_held_reading, base_extractiveness, 36, 0.28).
narrative_ontology:measurement_basis(elec_be_t36, observed).
narrative_ontology:measurement(elec_be_t48, electronic_money_emergence__first_held_reading, base_extractiveness, 48, 0.3).
narrative_ontology:measurement_basis(elec_be_t48, observed).
narrative_ontology:measurement(elec_be_t60, electronic_money_emergence__first_held_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(elec_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(elec_su_t0, observed).
narrative_ontology:measurement(elec_su_t12, electronic_money_emergence__first_held_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement_basis(elec_su_t12, observed).
narrative_ontology:measurement(elec_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement_basis(elec_su_t24, observed).
narrative_ontology:measurement(elec_su_t36, electronic_money_emergence__first_held_reading, suppression_requirement, 36, 0.21).
narrative_ontology:measurement_basis(elec_su_t36, observed).
narrative_ontology:measurement(elec_su_t48, electronic_money_emergence__first_held_reading, suppression_requirement, 48, 0.22).
narrative_ontology:measurement_basis(elec_su_t48, observed).
narrative_ontology:measurement(elec_su_t60, electronic_money_emergence__first_held_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(elec_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint decomposes the natural-language 'when did digital money emerge' question into three ε-invariant constraints per the ε-invariance principle. This file (first_held_reading) claims moderate, narrative-priority-driven extraction (ε≈0.31) anchored to institutional custody. The became_thinkable_reading sibling would author lower extraction (a conceptual/technical threshold has fewer identifiable institutional beneficiaries able to capture regulatory priority) and likely classifies closer to rope or mountain. The m4_m5_collapse_reading sibling would author extraction concentrated on statistical-agency and central-bank reporting conventions rather than settlement-bank custody, with a different beneficiary set (statisticians/central-bank data administrators) and likely a piton or tangled_rope profile tied to measurement-convention inertia. All three share the underlying kernel but are linked, not merged, per Rule 1.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
