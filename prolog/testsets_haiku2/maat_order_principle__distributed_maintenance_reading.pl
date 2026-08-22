% ============================================================================
% CONSTRAINT STORY: maat_order_principle__distributed_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__distributed_maintenance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: maat_order_principle__distributed_maintenance_reading
 *   human_readable: Ma'at Maintenance as Distributed Cosmic Responsibility
 *   domain: religious/political
 *
 * SUMMARY:
 *   This constraint instantiates the distributed maintenance reading of the
 *   Ma'at order principle: cosmic order is sustained by every actor—Pharaoh,
 *   nobility, priests, and commons—conducting themselves properly within
 *   their assigned station. No single person embodies Ma'at; instead, Ma'at
 *   emerges from the collective maintenance of proper conduct across all
 *   levels. This reading contrasts with the divine mandate reading (Ma'at
 *   flows through Pharaoh from the cosmos and the Pharaoh cannot violate it)
 *   and the reciprocity reading (Ma'at imposes mutual obligations binding
 *   Pharaoh and subjects). The distributed reading emphasizes multiple
 *   legitimate interpreters, accountability distributed across the hierarchy,
 *   and the lowest structural extraction of the three readings. The
 *   constraint is claimed as rope (genuine coordination solving a collective
 *   action problem) and the metrics are authored to reflect that claim,
 *   though the author recognizes the reading's vulnerability to capture by
 *   both Pharaonic absolutism and priestly gatekeeping.
 *
 * KEY AGENTS:
 *   - Pharaoh: principal coordinator but subject to same cosmic principle as all actors; authority derives from demonstrated maintenance, not from inherent transcendence
 *   - Priesthood (organized interpreters): legitimate authority grounded in expertise and community consensus rather than Pharaonic delegation; gatekeeping risk for extractive capture
 *   - Nobility and officials: powerful stakeholders bearing accountability for Ma'at maintenance in their jurisdictions; constrained but not powerless
 *   - Common Egyptians: trapless stakeholders bearing distributed obligation with asymmetric accountability; no formal voice in interpretation
 *   - Foreign powers: structurally excluded from Ma'at system by definition of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__distributed_maintenance_reading, 0.31).
domain_priors:suppression_score(maat_order_principle__distributed_maintenance_reading, 0.28).
domain_priors:theater_ratio(maat_order_principle__distributed_maintenance_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(maat_order_principle__distributed_maintenance_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__distributed_maintenance_reading, rope).
narrative_ontology:human_readable(maat_order_principle__distributed_maintenance_reading, "Ma'at Maintenance as Distributed Cosmic Responsibility").
narrative_ontology:topic_domain(maat_order_principle__distributed_maintenance_reading, "religious/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__distributed_maintenance_reading, '9da3509f-8060-4e7b-b88a-db6ea40dfbe8').
narrative_ontology:cs_kernel_codification('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', distributed).
narrative_ontology:cs_authority_grounding('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', practice).
narrative_ontology:cs_interpretation_layer_present('9da3509f-8060-4e7b-b88a-db6ea40dfbe8').
narrative_ontology:cs_reading_relation('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', maat_order_principle__divine_mandate_reading, coexists_with).
narrative_ontology:cs_reading_relation('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', maat_order_principle__reciprocity_reading, influences).
narrative_ontology:cs_axiom('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', foundational, cosmic_maintenance_distributed_across_hierarchy).
narrative_ontology:cs_axiom_status(cosmic_maintenance_distributed_across_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', cosmic_maintenance_distributed_across_hierarchy, conventional).
narrative_ontology:cs_axiom('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', secondary, priestly_interpretation_accountable_to_consensus).
narrative_ontology:cs_axiom_status(priestly_interpretation_accountable_to_consensus, holdable).
narrative_ontology:cs_axiom_grounding('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', priestly_interpretation_accountable_to_consensus, conventional).
narrative_ontology:cs_reference_frame('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', collective_cosmic_maintenance).
narrative_ontology:cs_drift_state('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', late_dynastic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9da3509f-8060-4e7b-b88a-db6ea40dfbe8', '').
narrative_ontology:cs_kernel_id(maat_order_principle__distributed_maintenance_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, egyptian_society_collectively).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, priestly_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, nobility_and_officials).
narrative_ontology:constraint_beneficiary(maat_order_principle__distributed_maintenance_reading, common_egyptians).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, nobility_and_officials).
narrative_ontology:constraint_victim(maat_order_principle__distributed_maintenance_reading, common_egyptians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds office as the principal coordinator of Ma'at maintenance but is bound by the same obligation as any other actor: proper conduct sustains cosmic order. The Pharaoh cannot transcend Ma'at through office alone; the legitimacy of rule rests entirely on demonstrated maintenance. Must perform ritual obligations, dispense justice, and manage resources according to cosmic principle. Failure damages the cosmic order and delegitimizes rule.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, pharaoh, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, pharaoh, beneficiary).

% Interpret and teach Ma'at principle; conduct rituals; assess whether Pharaoh and common actors maintain proper conduct. Their interpretive authority derives from demonstrated expertise in cosmic principle, not from inherent status. They benefit from a stable cosmological framework and from recognition as legitimate interpreters, but their authority is perpetually under evaluation by results and consensus rather than permanently secured.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priestly_interpreters, beneficiary,
    organized, generational, constrained, national).

% Bear responsibility for maintaining Ma'at within their jurisdictions and households. Their power and wealth are legitimate only insofar as they conduct themselves according to cosmic principle; failure to do so undermines their authority and invites replacement. They benefit from a stable cosmic order that protects their standing but carry the burden of demonstrating continued fitness through proper conduct.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, nobility_and_officials, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, nobility_and_officials, beneficiary).

% Bear responsibility for maintaining Ma'at through proper conduct in their stations: farmers through just labor, artisans through skilled work, servants through loyalty and obedience. They have no formal voice in interpretation or policy, no exit option from the cosmic obligation. They benefit from a stable order in which all actors maintain their roles but carry unequal burden in accountability—their failures are visible to their lords, while elite failures are often rationalized by priestly interpreters.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, common_egyptians, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__distributed_maintenance_reading, common_egyptians, beneficiary).

% Serve as secondary interpreters and witnesses to Ma'at maintenance across temples and communities. They hold less authority than the central priesthood but carry distributed responsibility for monitoring cosmic order. Their position is analytical rather than agenda-setting; they report conditions to higher authority and interpret principles locally but do not set policy.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, priesthood_broader_community, observer,
    organized, generational, constrained, national).

% Are outside the Ma'at system by definition—they do not participate in Egyptian cosmic order maintenance and are treated as forces of disorder (Isfet). Would argue that their own cosmologies are equally valid and that Egyptian Ma'at is a provincial reading, not a universal principle. Their exclusion is structural to the reading: one cannot simultaneously maintain Ma'at and acknowledge foreign cosmological equivalence.
narrative_ontology:constraint_stakeholder(maat_order_principle__distributed_maintenance_reading, foreign_adversaries, excluded,
    powerful, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__distributed_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(maat_order_principle__distributed_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes cosmic maintenance responsibility across all social levels according to station: Pharaoh maintains the cosmic order through ritual and governance; nobility through just administration; priests through interpretation and ritual; commons through proper conduct in assigned roles. This solves the coordination problem of sustaining cosmic order without concentrating interpretive or maintenance authority in a single person or class.
% TRANSFER_FUNCTION: Transfers authority to interpret Ma'at from the Pharaoh's personal will to demonstrated competence and priestly-community consensus. Also transfers accountability: failure by any actor—from Pharaoh downward—creates cosmic imbalance, making maintenance a reciprocal obligation rather than a top-down command. What flows is not wealth or labor primarily, but responsibility and legitimacy.
% ABSENT_VOICES: Foreign powers and external cosmologies are structurally excluded; they would contest the universality of Ma'at and argue for pluralism. Also largely absent: women's voices in formal interpretation (though women performed maintenance roles), and the views of those suffering under elite failure (commoners whose lords violated Ma'at had little recourse beyond appeal to higher authority or hope for cosmic correction).
% DISAPPEARANCE_RATIONALE: If the distributed maintenance principle vanished and were replaced by either pure divine mandate (only Pharaoh interprets) or pure reciprocity (only mutual obligation binds), the entire legitimacy structure would shift. Without distributed responsibility, Pharaoh could not be held accountable by priestly consensus, or commoners could claim exemption from cosmic obligation. The cosmological justification for the entire social structure would collapse or require radical reformulation.
% FOUNDING_PROBLEM: Early Egyptian society faced the fundamental problem of justifying social hierarchy and coordinating collective behavior across vast distances and time: why should the powerless accept subordination, and why should the powerful exercise restraint? Ma'at as distributed maintenance provides the answer: all actors maintain cosmic order through proper conduct in their stations, and violation by anyone—Pharaoh included—threatens the whole. This justifies hierarchy while binding it to principle.
% FOUNDING_PROBLEM_CORROBORATION: Priestly texts and Pharaonic inscriptions attest the founding problem and the distributed reading. However, evidence of Pharaonic abuse, elite corruption, and commoner resistance suggests the problem was never fully resolved; skeptics (historians, later philosophical traditions) attest that the reading was aspirational rather than operative—a coordination principle that failed in practice. Both accounts are corroborated from outside the most benefiting party (the Pharaonic line itself).
narrative_ontology:disappearance_verdict(maat_order_principle__distributed_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__distributed_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__distributed_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__distributed_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__distributed_maintenance_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__distributed_maintenance_reading_tests).
:- end_tests(maat_order_principle__distributed_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31) because the distributed reading claims genuine coordination—all actors benefit from cosmic stability and participate in its maintenance. There is no designated capturer; the principle does not concentrate rents in one seat. Suppression is similarly low (0.28) because the principle is theoretically self-enforcing—actors maintain Ma'at voluntarily to avoid cosmic and social consequences, not through coerced compliance. Theater ratio (0.22) reflects the reading's structural vulnerability: the performative element rises as elites rationalize violations as 'maintenance' and priests perform rituals while ignoring actual cosmic imbalance. The time series shows slight drift upward in extractiveness and theater, suggesting the reading's degradation over time as gatekeeping and rationalization increase—but the drift is modest because the core principle remains appealing and genuinely coordinates across centuries. Accessibility collapse is moderate-high (0.68) because once the distributed principle is understood, the alternatives (cosmological pluralism, rejection of cosmic obligation) become cognitively available but practically foreclosed by the entire institutional structure. Resistance is moderate (0.42) because the principle faces real opposition—from those suffering unequal accountability and from those whose cosmologies reject Ma'at—but the opposition is diffuse and unorganized rather than concentrated and coordinated.
 *
 * PERSPECTIVAL GAP:
 *   The payer (nobility and commons) and agenda-setter (Pharaoh and priesthood) seats compute differently because the principle claims to bind all equally but gives unequal interpretive authority and unequal accountability mechanisms. From the Pharaoh's seat, the distributed reading is genuine coordination that legitimates rule through principle. From the commons' seat, the distributed reading is a principle that justifies their subordination while exempting elites from equivalent consequences. The priesthood's seat is intermediate but gatekeeping—they benefit from interpretation authority that the distributed reading confers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: egyptian_society_collectively (all benefit from stable cosmic order) and priestly_interpreters (benefit from gatekeeping authority and resource flows). The designation of 'egyptian_society_collectively' as beneficiary masks the internal divergence—this is the coordination function the reading claims, but it obscures whether the distribution of benefit is actually symmetrical. Victims: none are designated, because the distributed reading claims no victim set; however, common Egyptians bearing asymmetric accountability and having no exit option constitute a structural victim position that the reading does not acknowledge. This is the key vulnerability: the reading's claim of distributed responsibility masks asymmetric vulnerability. Directionality for the Pharaoh: d ~0.2 (strong beneficiary—authority and legitimacy are contingent on the principle but are heavily secured by it). Directionality for priesthood: d ~0.25 (beneficiary through gatekeeping, but constrained by accountability to maintain credibility). Directionality for nobility: d ~0.45 (intermediate—powerful but contingent). Directionality for commons: d ~0.75 (heavy target—trapped, unequal accountability, no voice).
 *
 * MANDATROPHY ANALYSIS:
 *   The distributed maintenance reading faces the mandatrophy risk endemic to principle-based legitimacy systems: as the founding problem (need for cosmic justification of hierarchy) is solved, the principle can persist as theater—Pharaohs perform maintenance rituals while violating the spirit; priests interpret violations as maintenance; commons perform their roles while elites accumulate without cosmic consequence. The measurement data shows slow drift toward higher theater ratio, suggesting gradual degradation from genuine coordination into ritualized performance. However, mandatrophy is not yet resolved by the interval's end (0.31 extractiveness, 0.22 theater is not the piton profile). The constraint remains a genuine rope with increasing theater, not yet a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distributed_vs_hierarchical_authority,
    'Is the ''distributed'' reading genuinely distinct from a reading where the Pharaoh monopolizes interpretation through the priesthood as a bureaucratic tool?',
    'Examination of priestly independence from Pharaonic control, instances of priestly correction of Pharaonic conduct, and whether priestly interpretation could contradict Pharaonic claims without persecution. Historical record of priest-Pharaoh conflicts.',
    'If priests were truly autonomous interpreters, the distributed reading is coherent and extractiveness is low (genuine coordination). If priests merely rationalized Pharaonic will, the reading collapses into divine mandate and extractiveness is much higher (pure extraction masked by priestly language).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_vs_hierarchical_authority, empirical, 'Whether the distributed maintenance reading permits genuine priestly independence or priesthood functions as Pharaonic extension.').

omega_variable(
    commoner_accountability_asymmetry,
    'Did the distributed maintenance principle genuinely hold commoners accountable for Ma''at maintenance in the same structural way it held elites and Pharaoh accountable, or was commoner obligation purely downward with no recourse?',
    'Historical evidence of commoner legal action against elite violation of Ma''at, divine punishment of elite for cosmic imbalance, or priesthood enforcing accountability equally across classes. Absence of such evidence would indicate accountability is asymmetric.',
    'Asymmetric accountability would indicate the reading is a cover story for elite extraction—the principle claims universal obligation but only enforces it against the powerless. This would shift the constraint from rope (genuine coordination) toward snare or tangled_rope (extraction masked by principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commoner_accountability_asymmetry, empirical, 'Whether Ma''at maintenance principle created symmetric or asymmetric accountability.').

omega_variable(
    reading_vs_divine_mandate_boundary,
    'Is the distributed maintenance reading logically compatible with the divine mandate reading, or do they foreclose each other when held within a single framework?',
    'Textual analysis of whether a Pharaoh can simultaneously embody Ma''at by cosmic fiat (divine mandate) and be subject to accountability by priestly-community consensus (distributed maintenance). Exploration of whether Egyptian priestly practice accommodated both readings simultaneously or held them as incompatible positions.',
    'If compatible (coexists_with), both readings remain live and the constraint family is syncretic. If incompatible (forecloses), the distributed reading logically rules out the divine mandate as a coherent position, and the two readings form an either/or foundation choice rather than compatible framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_divine_mandate_boundary, conceptual, 'Logical relationship between distributed and divine-mandate readings of Ma''at.').

omega_variable(
    extraction_via_priestly_gatekeeping,
    'Does the priesthood''s monopoly on Ma''at interpretation constitute a form of extraction—a mechanism by which priests maintain authority and resources by controlling access to cosmic legitimacy?',
    'Analysis of whether priestly interpretation served primarily as a coordination mechanism (enabling distributed maintenance) or as a gatekeeping mechanism (enabling elite capture of legitimacy). Evidence of priestly accumulation of wealth and status justified by interpretive authority.',
    'If priestly gatekeeping is primarily extractive, the constraint should be reclassified as tangled_rope (coordination function for society, extraction mechanism for priests) or snare (if priestly control is coercive and alternatives are suppressed). The extracted value would shift from the designated beneficiary (egyptian_society_collectively) to the actual capturer (priestly_interpreters).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_priestly_gatekeeping, empirical, 'Whether priestly interpretation of Ma''at functions as coordination or extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__distributed_maintenance_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__distributed_maintenance_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(maat_tr_t0, observed).
narrative_ontology:measurement(maat_tr_t5, maat_order_principle__distributed_maintenance_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement_basis(maat_tr_t5, observed).
narrative_ontology:measurement(maat_tr_t10, maat_order_principle__distributed_maintenance_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(maat_tr_t10, observed).
narrative_ontology:measurement(maat_tr_t15, maat_order_principle__distributed_maintenance_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(maat_tr_t15, observed).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__distributed_maintenance_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(maat_tr_t20, observed).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__distributed_maintenance_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(maat_tr_t25, observed).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__distributed_maintenance_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(maat_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(maat_be_t0, observed).
narrative_ontology:measurement(maat_be_t5, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement_basis(maat_be_t5, observed).
narrative_ontology:measurement(maat_be_t10, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement_basis(maat_be_t10, observed).
narrative_ontology:measurement(maat_be_t15, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 15, 0.31).
narrative_ontology:measurement_basis(maat_be_t15, observed).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(maat_be_t20, observed).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(maat_be_t25, observed).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__distributed_maintenance_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(maat_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(maat_su_t0, observed).
narrative_ontology:measurement(maat_su_t5, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 5, 0.26).
narrative_ontology:measurement_basis(maat_su_t5, observed).
narrative_ontology:measurement(maat_su_t10, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 10, 0.27).
narrative_ontology:measurement_basis(maat_su_t10, observed).
narrative_ontology:measurement(maat_su_t15, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 15, 0.28).
narrative_ontology:measurement_basis(maat_su_t15, observed).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement_basis(maat_su_t20, observed).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement_basis(maat_su_t25, observed).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__distributed_maintenance_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(maat_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__distributed_maintenance_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(maat_order_principle__distributed_maintenance_reading, 0.12).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__divine_mandate_reading).
narrative_ontology:affects_constraint(maat_order_principle__distributed_maintenance_reading, maat_order_principle__reciprocity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the maat_order_principle kernel. The distributed_maintenance_reading instantiates Ma'at as a principle coordinating all actors across the hierarchy through distributed responsibility and multiple legitimate interpreters. Sibling readings—divine_mandate_reading and reciprocity_reading—instantiate different structural mechanisms for the same kernel. All three share the kernel (Ma'at as cosmic principle) but diverge on authority, accountability, and extraction mechanisms. Each reading authorizes different constraint stories with different ε values, beneficiary structures, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__distributed_maintenance_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
