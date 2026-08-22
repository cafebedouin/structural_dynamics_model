% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections via International Guarantor Mechanism
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   The Lausanne Treaty (1923) embedded minority protections as international
 *   obligations, nominally supervised by guarantor states and later by
 *   European human rights bodies. The guarantor reading interprets these
 *   protections as enforceable through external diplomacy and international
 *   court review, not solely through Turkey's domestic legal system. This
 *   reading claims that minorities have standing to appeal to the ECHR and
 *   that guarantor states retain the right to diplomatic intervention on
 *   minority-rights questions. It establishes an international supervisory
 *   pathway for minority protections, creating external pressure on Turkey's
 *   domestic minority policy. The constraint's extractiveness is
 *   low-to-moderate (0.38 at interval end) because it creates leverage for
 *   minorities without direct enforcement power; its theater ratio rises over
 *   time (0.25 to 0.52), indicating that guarantor-state enforcement
 *   increasingly performs diplomatic concern rather than producing remedies.
 *   The guarantor reading is contestable: a restrictive reading claims
 *   Lausanne protects only individual worship, not institutional autonomy; an
 *   expansive reading claims Lausanne guarantees functional continuity of
 *   pre-1923 religious governance. This JSON instantiates the guarantor
 *   reading as a scaffold—a transitional arrangement that creates supervisory
 *   machinery without binding enforcement, functioning as diplomatic leverage
 *   on Turkish policy rather than as a binding constraint on state behavior.
 *
 * KEY AGENTS:
 *   - Turkey as signatory state: formally bound by Lausanne but controls domestic implementation
 *   - Religious minority communities (Greek Orthodox, Armenian, Jewish): beneficiaries of external appeal pathway but bear litigation costs and uncertain remedies
 *   - Guarantor states (France, Italy, Greece, UK): retain diplomatic enforcement rights but lack direct legal mechanisms
 *   - European Court of Human Rights: interprets and adjudicates minority claims, issues non-binding-except-for-reputation rulings
 *   - Turkish nationalist framing: excluded from the guarantor framework, politically powerful domestically
 *   - Turkish domestic courts: bear the burden of revisiting settled cases under international pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.38).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.29).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections via International Guarantor Mechanism").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'edb393dd-1052-4bd2-aefb-00e83255d237').
narrative_ontology:cs_kernel_codification('edb393dd-1052-4bd2-aefb-00e83255d237', fixed_text).
narrative_ontology:cs_authority_grounding('edb393dd-1052-4bd2-aefb-00e83255d237', lineage).
narrative_ontology:cs_interpretation_layer_present('edb393dd-1052-4bd2-aefb-00e83255d237').
narrative_ontology:cs_reading_relation('edb393dd-1052-4bd2-aefb-00e83255d237', lausanne_minority_protections__restrictive_reading, influences).
narrative_ontology:cs_reading_relation('edb393dd-1052-4bd2-aefb-00e83255d237', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('edb393dd-1052-4bd2-aefb-00e83255d237', foundational, international_supervisory_authority_binding).
narrative_ontology:cs_axiom_status(international_supervisory_authority_binding, holdable).
narrative_ontology:cs_axiom_grounding('edb393dd-1052-4bd2-aefb-00e83255d237', international_supervisory_authority_binding, conventional).
narrative_ontology:cs_axiom('edb393dd-1052-4bd2-aefb-00e83255d237', foundational, minority_external_appeal_right).
narrative_ontology:cs_axiom_status(minority_external_appeal_right, holdable).
narrative_ontology:cs_axiom_grounding('edb393dd-1052-4bd2-aefb-00e83255d237', minority_external_appeal_right, deontological).
narrative_ontology:cs_reference_frame('edb393dd-1052-4bd2-aefb-00e83255d237', international_supervisory_framework).
narrative_ontology:cs_drift_state('edb393dd-1052-4bd2-aefb-00e83255d237', contemporary_post_echr_integration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('edb393dd-1052-4bd2-aefb-00e83255d237', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, european_human_rights_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, religious_minority_communities).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary state bound by Lausanne Treaty obligations, Turkey formally commits to respecting minority rights. Under the guarantor reading, it faces external supervisory review and diplomatic pressure from guarantor states and European human rights bodies. It administers the constraint but does not control its interpretation or adjudication unilaterally.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkey_as_signatory_state, agenda_setter,
    institutional, civilizational, constrained, national).

% Greek Orthodox, Armenian, Jewish, and other non-Muslim communities inside Turkey possess rights to worship, property, and institutional autonomy under Lausanne protections. The guarantor reading grants them access to external appeal mechanisms (European Court of Human Rights, diplomatic intervention) to challenge domestic restrictions. They bear the cost of protracted litigation and diplomatic negotiation, with uncertain remedies.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, religious_minority_communities, beneficiary,
    organized, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, religious_minority_communities, payer).

% France, Italy, Greece, UK, and other signatories to Lausanne retain the right to diplomatically enforce minority protections against Turkey. They bring cases to international bodies, lodge formal objections, and condition trade or security cooperation on compliance. Their leverage is diplomatic and reputational rather than direct legal enforcement.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_states, agenda_setter,
    institutional, civilizational, mobile, global).

% Interprets and adjudicates minority rights claims through the European Convention on Human Rights, often reading Lausanne protections into the Convention's guarantees. Issues rulings that create binding precedent for Turkey but lacks direct enforcement machinery; compliance depends on Turkish domestic implementation and guarantor state pressure.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_court_of_human_rights, agenda_setter,
    institutional, civilizational, analytical, continental).

% Voices and institutions that contest external supervision of minority treatment, viewing guarantor-reading enforcement as neo-colonial interference and restrictions on Turkey's sovereign right to define its own religious law and national cohesion. Excluded from the guarantor-reading framework but present as domestic political resistance to international oversight.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_nationalist_framing, excluded,
    powerful, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(lausanne_minority_protections__guarantor_reading, turkish_nationalist_framing).

% Must adjudicate minority disputes under Turkish law while knowing their decisions are subject to international review and override. Bear the administrative burden of revisiting settled cases when international bodies issue contradictory rulings, and face pressure from both Turkey's executive (domestic sovereignty rhetoric) and international bodies (minority protection mandates).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, domestic_turkish_courts, payer,
    institutional, generational, constrained, national).

% Academic and policy-analysis communities track how Lausanne protections are interpreted across different reading frameworks and how guarantor enforcement compares to other minority-rights regimes (EU accession, UN mechanisms, bilateral treaties).
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, observer_comparative_jurisprudence, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a supervisory pathway allowing international guarantor states and European human rights bodies to review and challenge Turkey's treatment of religious minorities against the standard of Lausanne protections, creating a forum for dispute resolution that is not confined to Turkish domestic law.
% TRANSFER_FUNCTION: Transfers interpretive authority and remedial power from exclusively Turkish domestic courts and the Turkish state to a hybrid system where external bodies (guarantor states, ECHR, international diplomacy) can override, revise, or pressure Turkish decisions on minority rights, moving the authority boundary outward.
% ABSENT_VOICES: Turkish nationalist and Islamist constituencies that view guarantor-reading enforcement as threatening Turkish sovereignty and national cohesion are structurally excluded from the international supervisory framework, though they hold significant domestic political power and shape Turkey's defensive posture against external intervention.
% DISAPPEARANCE_RATIONALE: If the guarantor-reading enforcement mechanism disappeared overnight—leaving only Turkish domestic courts to adjudicate minority claims under Turkish law—minorities would lose their international appeal pathway and Turkey would recover unilateral control over minority-rights interpretation. Guarantor states would lose the diplomatic lever they currently wield to pressure compliance.
% FOUNDING_PROBLEM: After the 1923 population exchange, religious minorities remaining in Turkey lacked international protection against unilateral domestic majoritarian reinterpretation of their rights. The Lausanne Treaty attempted to lock in protections via international guarantor-state oversight.
% FOUNDING_PROBLEM_CORROBORATION: European human rights bodies and guarantor states attest the founding problem remains live—Turkish minorities continue to face domestic restrictions on property, worship, and institutional autonomy that the guarantor reading interprets as Lausanne violations. Turkish state and nationalist commentators attest the problem is outdated; Turkish courts claim to apply Lausanne protections domestically without external supervision. Academic comparative law scholarship, refugee testimony from minorities, and documented property disputes corroborate the guarantor states' reading.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the guarantor reading creates an external supervisory pathway that shifts interpretive authority outward from Turkey's unilateral control, but lacks direct enforcement mechanisms to compel compliance. Turkey can ignore ECHR rulings or guarantor-state complaints; compliance depends on reputational cost and diplomatic pressure, not binding adjudication. Theater ratio rises steadily from 0.25 (1923, when guarantor enforcement was active diplomacy backed by geopolitical leverage) to 0.52 (2026, when enforcement is largely performative—rulings issued, statements made, but remedies slow or absent). This rise indicates that guarantor-state supervision has increasingly become a ritual of international concern rather than a mechanism that reliably produces minority-rights improvements. Suppression is low (0.29) because the guarantor reading does not coerce minorities into accepting restrictions; minorities can invoke external mechanisms. The readings are measured on a shared time grid (every metric at every time point) so that temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   The guarantor state seat and the Turkish state seat should compute differently: from the guarantor states' position, the external supervisory pathway is genuine coordination that protects minorities from unilateral Turkish reinterpretation; from Turkey's position, the mechanism is external constraint on its sovereignty that threatens national cohesion. The minorities themselves sit asymmetrically: they benefit from external appeal access but bear the litigation burden and uncertainty of international remedies. The engine computes this divergence from the structural data—the guarantor reading's core premise (external supervision creates enforceable obligations) will be seen from Turkish and guarantor seats very differently, even though both are reading the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Guarantor states are structural beneficiaries (they retain diplomatic leverage and soft-power appeal mechanisms). Religious minorities are beneficiaries (access to external appeal) and secondary payers (protracted litigation, uncertain remedies, social costs of being classified as needing 'protection'). Turkey is the primary payer—it bears external supervisory pressure and constraints on its unilateral interpretive authority. The constraint's directionality depends on one's position: from the guarantor seat, d is low (leverage retained); from the Turkish seat, d is high (sovereignty constrained); from the minority seat, d is moderate (access granted but remedy uncertain).
 *
 * MANDATROPHY ANALYSIS:
 *   The guarantor reading avoids mandatrophy through the scaffold framing: it does not claim to enforce minority rights directly, but rather to create a supervisory pathway that can pressure Turkey through diplomacy. The measured rise in theater ratio (0.25 to 0.52) indicates increasing performativity—the supervisory machinery persists but becomes less functionally tied to minority-rights outcomes. The absent sunset clause reflects the permanent tension: the reading was created to lock in protections (Lausanne, 1923) but lacks the enforcement mechanism to impose mandated outcomes. The founding problem (minorities lack international protection against Turkish reinterpretation) remains contested: guarantor states and ECHR attest it is live; Turkish state and nationalist voices attest it is obsolete. This contestation is structural, not resolvable by better interpretation—it is the kernel itself that is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supervisory_mechanism_effectiveness,
    'Does the guarantor-reading supervisory pathway (ECHR rulings, guarantor-state diplomacy) actually produce minority-rights improvements in Turkey, or does it function primarily as a reputational signal?',
    'Temporal analysis of ECHR rulings on Turkish minority cases: do rulings result in changed Turkish law within 2 years? Do minorities experience measurable improvements in property rights, institutional autonomy, or worship access post-ruling? Comparison with cases in restrictive-reading jurisdictions.',
    'If supervisory mechanisms produce measurable improvements, the constraint is genuinely scaffold-like (temporary leverage toward a functional goal); if they produce only performative compliance and delayed remedies, the constraint is closer to a piton (preserved through international theater, not functional outcome).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supervisory_mechanism_effectiveness, empirical, 'Whether guarantor-reading enforcement produces functional minority-rights improvements or operates as international theater.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the guarantor reading logically foreclose the restrictive reading (individual-worship-only), or do they coexist as live positions held by different institutional actors?',
    'Examine whether Turkey''s domestic courts could simultaneously adopt the restrictive reading and comply with ECHR guarantor-reading precedent. If simultaneous adoption is logically impossible within a single institutional framework, the readings foreclose; if different institutions can hold each independently, they coexist.',
    'If readings foreclose, the kernel is structurally unstable and will eventually collapse into one reading; if they coexist, the kernel is stable and the contest is permanent. This determines whether the guarantor reading is a transient reformist position or a stable alternative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether guarantor and restrictive readings of Lausanne are logically incompatible or simply different institutional positions.').

omega_variable(
    guarantor_state_interest_stability,
    'Does guarantor-state diplomatic enforcement of minority protections remain stable as geopolitical interests (trade, security, NATO alignment) shift, or does enforcement weaken when Turkey is strategically valuable?',
    'Historical tracking: do guarantor states pressure Turkey on minority rights when Turkey is isolated vs. when Turkey is strategically aligned? Timeline of ECHR cases, diplomatic statements, and trade/security cooperation across Cold War, post-Cold War, and contemporary periods.',
    'If enforcement is stable, the guarantor reading creates a durable constraint on Turkish sovereignty; if enforcement is contingent on geopolitical alignment, the supervisory mechanism is conditional and the constraint''s actual extractiveness (directional d from guarantor seat) is lower than the reading claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guarantor_state_interest_stability, empirical, 'Whether guarantor-state enforcement of minority protections is stable across geopolitical shifts or contingent on strategic interests.').

omega_variable(
    theater_ratio_trend_interpretation,
    'Does the rising theater ratio (0.25 to 0.52) indicate that guarantor-reading enforcement is becoming increasingly performative, or does it indicate that the supervisory machinery itself has shifted to a less coercive (more diplomatic, less legal) form?',
    'Distinguish types of guarantor activity: direct legal intervention (ECHR cases, binding rulings) vs. diplomatic pressure (statements, bilateral negotiation, conditional aid). Compare ratios across activity types over the interval.',
    'If rising theater indicates performance without functional remedy, the constraint is degrading toward piton-like status; if rising theater indicates a conscious shift to less coercive supervisory forms, the constraint remains functionally a scaffold with changing enforcement style.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_trend_interpretation, empirical, 'Whether increasing theater ratio indicates constraint degradation toward piton or a shift to less coercive supervisory forms.').

omega_variable(
    guarantor_reading_vs_expansive_reading_influence,
    'Does the guarantor reading''s emphasis on external supervisory enforcement influence (create structural pressure on) the expansive reading''s claim that Lausanne guarantees institutional self-administration, or do the readings operate independently?',
    'Track whether actors who adopt the guarantor reading''s supervisory framing also adopt the expansive reading''s institutional autonomy claims, or whether guarantor-reading advocates argue for narrower supervisory authority focused on individual worship. Compare across institutional actors (ECHR, guarantor states, Turkish minority communities, Turkish state).',
    'If the readings are linked (guarantor reading influences expansive reading), then the constraint family is structurally coupled—changes in guarantor enforcement shape expansive-reading claims; if independent, the readings are orthogonal and can be analyzed separately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guarantor_reading_vs_expansive_reading_influence, conceptual, 'Whether the guarantor reading''s external supervisory framing structurally influences claims about institutional autonomy in the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.25).
narrative_ontology:measurement(laus_tr_t1950, lausanne_minority_protections__guarantor_reading, theater_ratio, 1950, 0.32).
narrative_ontology:measurement(laus_tr_t1980, lausanne_minority_protections__guarantor_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(laus_tr_t2000, lausanne_minority_protections__guarantor_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(laus_tr_t2015, lausanne_minority_protections__guarantor_reading, theater_ratio, 2015, 0.51).
narrative_ontology:measurement(laus_tr_t2026, lausanne_minority_protections__guarantor_reading, theater_ratio, 2026, 0.52).

% Extraction over time
narrative_ontology:measurement(laus_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.15).
narrative_ontology:measurement(laus_be_t1950, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(laus_be_t1980, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1980, 0.31).
narrative_ontology:measurement(laus_be_t2000, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(laus_be_t2015, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement(laus_be_t2026, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.18).
narrative_ontology:measurement(laus_su_t1950, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1950, 0.21).
narrative_ontology:measurement(laus_su_t1980, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(laus_su_t2000, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(laus_su_t2015, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2015, 0.29).
narrative_ontology:measurement(laus_su_t2026, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2026, 0.29).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(lausanne_minority_protections__guarantor_reading, 0.18).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% The Lausanne minority protections kernel decomposes into three structurally distinct constraints. The guarantor reading (this constraint) interprets Lausanne obligations as enforceable through international guarantor-state diplomacy and ECHR review; the restrictive reading (sibling) limits protections to individual worship under domestic law; the expansive reading claims Lausanne guarantees institutional self-administration and pre-1923 governance continuity. These are not different measurements of the same constraint—they are three different constraints with different ε values, different beneficiary/victim structures, and different enforcement mechanisms. All three readings operate within the same kernel (Lausanne Treaty text) but read that kernel as instantiating fundamentally different obligations. The guarantor reading is positioned upstream of both siblings: guarantor-reading enforcement creates international legal precedent and reputational pressure that shapes how the restrictive and expansive readings are invoked and defended domestically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
