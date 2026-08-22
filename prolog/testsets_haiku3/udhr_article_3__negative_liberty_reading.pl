% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: Article 3 Negative Liberty Reading: State Prohibition on Arbitrary Deprivation
 *   domain: constitutional_law/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the negative liberty reading of
 *   Article 3 of the Universal Declaration of Human Rights. The reading
 *   anchors the right to life and liberty in a prohibition on arbitrary state
 *   deprivation—specifically, it forbids capital punishment, torture, and
 *   indefinite detention without narrow due process, regardless of security
 *   justification. The reading treats individual protection from state
 *   violence as a foundational constraint that collective security measures
 *   cannot override. This is one reading of a contested kernel; sibling
 *   readings (positive entitlement, procedural hybrid) would instantiate
 *   different constraints with different beneficiary structures and different
 *   interpretations of what Article 3 requires.
 *
 * KEY AGENTS:
 *   - individual_rights_bearers: Hold negative rights against state violence; cannot exit protection; trapped by citizenship.
 *   - state_security_apparatus: Institutional payer; prohibited from capital punishment, torture, indefinite detention; constrained in the modalities available for enforcement.
 *   - collective_security_constituency: Organized, distributed cost-bearer and diffuse beneficiary; trades security-through-force capacity for security-through-restraint.
 *   - due_process_institutions: Courts and legal doctrine that adjudicate the narrow procedural gate; agenda-setter; interprets what constitutes 'due process'.
 *   - positive_entitlement_reading_partisans: Excluded; would argue Article 3 mandates affirmative state provision (welfare, healthcare) not merely prohibition on violence.
 *   - comparative constitutional regimes: Observer seat; jurisdictions with variant readings demonstrate the contest is live and the reading is not inevitable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.28).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.19).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, mountain).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "Article 3 Negative Liberty Reading: State Prohibition on Arbitrary Deprivation").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights").

domain_priors:emerges_naturally(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73').
narrative_ontology:cs_kernel_codification('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', fixed_text).
narrative_ontology:cs_authority_grounding('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', lineage).
narrative_ontology:cs_interpretation_layer_present('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73').
narrative_ontology:cs_reading_relation('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', udhr_article_3__positive_entitlement_reading, forecloses).
narrative_ontology:cs_reading_relation('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', foundational, individual_negative_liberty_foundational).
narrative_ontology:cs_axiom_status(individual_negative_liberty_foundational, holdable).
narrative_ontology:cs_axiom_grounding('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', individual_negative_liberty_foundational, deontological).
narrative_ontology:cs_axiom('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', foundational, state_violence_as_constitutional_violation).
narrative_ontology:cs_axiom_status(state_violence_as_constitutional_violation, holdable).
narrative_ontology:cs_axiom_grounding('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', state_violence_as_constitutional_violation, deontological).
narrative_ontology:cs_reference_frame('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', universal_prohibition_on_arbitrary_deprivation).
narrative_ontology:cs_drift_state('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', contemporary_post_2015, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f5f6abf-c6dc-4b80-bae7-390cf0e3ed73', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_rights_bearers).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, constitutional_restraint_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, collective_security_constituency).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_constituency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the right to life and liberty, protected against arbitrary state deprivation. This reading secures them by anchoring that protection in a negative constraint on state power rather than affirmative state provision. They cannot exit the jurisdiction without forfeiting protection; the constraint's value lies in what the state cannot do to them, not what the state must provide.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_rights_bearers, beneficiary,
    powerless, biographical, trapped, national).

% Is structurally prohibited from exercising certain violent capacities (capital punishment, torture, indefinite detention without due process) that it views as operationally necessary for collective security. The constraint operates through exclusion of tools and methods rather than affirmative obligation. The apparatus bears the cost of abandoning certain enforcement modalities and accepting narrower grounds for detention and restraint.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, constrained, national).

% Bears the distributed cost of constraint on state violence (slower response times, higher recidivism risk under narrower detention grounds, reduced deterrent capacity). Also accrues diffuse benefits (living in a state that does not practice capital punishment or torture creates a social environment of diminished state arbitrariness). The reading treats collective security as subordinate to individual protection; security through restraint rather than through amplified state capacity.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_constituency, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, collective_security_constituency, beneficiary).

% Would argue that Article 3's protection of life requires affirmative state provision of material conditions (healthcare, food, housing, employment) necessary to live. This reading structurally forecloses that interpretation by anchoring Article 3 in negative liberty (freedom from state violence) rather than positive entitlements. Their voice is absent from constitutional interpretation communities that adopt this reading.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, positive_entitlement_reading_partisans, excluded,
    analytical, generational, analytical, universal).

% Courts, bar associations, and legal doctrine that adjudicate whether state deprivation meets the narrow procedural gate. They administer the constraint by testing state action against the narrowed definition. Their authority to interpret 'due process' and 'narrow procedural justice' determines when the constraint binds and when it yields.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, due_process_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Jurisdictions that adopt variant readings (positive entitlement, welfare-inclusive, security-balancing) embody alternative codifications of Article 3. Their existence demonstrates the reading is contestable, not inevitable, but does not falsify this reading within the jurisdictions that endorse it.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, comparative_constitutional_regimes, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, individual_rights_bearers).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, universal standard: the state cannot execute, torture, or indefinitely detain individuals without due process, regardless of security justification. This coordinates the scope of legitimate state violence across all agents and prevents a race-to-the-bottom where jurisdictions compete by expanding state power to attract security-conscious citizens.
% TRANSFER_FUNCTION: Transfers the capacity to exercise arbitrary state violence FROM the state apparatus TO individual-held negative rights (freedom from that violence). The state renounces certain enforcement modalities; individuals gain the assured prohibition on their use. Collective security measures that depend on those modalities (capital deterrence, expedited detention, coercive interrogation) are the cost of that transfer.
% ABSENT_VOICES: Victims of violent crimes whose perpetrators might have been executed under alternative readings; security officials who believe capital punishment deters and indefinite detention prevents recidivism; welfare advocates who read Article 3 as mandating affirmative provision (not merely prohibition). These parties are outside the interpretive community that endorses the negative liberty reading.
% DISAPPEARANCE_RATIONALE: If Article 3 were reinterpreted to permit capital punishment, torture, and indefinite detention in the name of security, the constitutional order would reorganize: some jurisdictions would reinstate capital punishment; interrogation regimes would expand; individual predicability about state power would collapse. The constraint's disappearance would not be a neutral policy shift but a rupture in the foundation of individual protection against state violence.
% FOUNDING_PROBLEM: The European state tradition of arbitrary execution, torture, and indefinite detention of political prisoners and minorities created a security apparatus unmoored from individual protection. Article 3 was adopted to establish that the state's power to take life and liberty is not boundless but constrained by the requirement of narrow due process—that is, to prevent state terror as a tool of rule.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship documents the fascist regimes' use of capital punishment, torture, and indefinite detention; international human rights organizations (Amnesty International, Human Rights Watch) monitor ongoing violations in jurisdictions that weaken these protections; legal scholars outside the benefiting parties attest that the founding problem—arbitrary state violence—persists wherever due process protections are eroded. The International Criminal Court's jurisdiction over crimes against humanity rests on the premise that unrestrained state violence remains a live threat.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, ExtMetricName, E),
    domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(udhr_article_3__negative_liberty_reading),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is CLAIMED as mountain (natural law, emerges from the structure of individual dignity and state authority) because the reading treats negative liberty as foundational—a structural fact about human agency rather than a policy choice. However, extractiveness measures 0.28, not near-zero as a genuine mountain would show. The measurement reflects that this reading, while presented as natural law, serves identifiable beneficiaries (individual rights-bearers, constitutional restraint doctrine) and imposes costs on state security apparatus and collective security. The gap between the claim (mountain) and the metrics (moderate extractiveness) is intentional and diagnostic: a false summit is one presented as natural law but benefiting identifiable parties. Suppression is low (0.19) because the reading's enforcement does not depend on coercion but on legal interpretation and institutional consensus; resistance is moderate (0.62) because security apparatus and some constituencies actively oppose capital punishment abolition and procedural constraints. Theater is very low (0.12) because the reading's performative component is minimal—due process is operationally real, not theatrical. Extractiveness has risen slowly from 1948 (0.15) to 2026 (0.28) as the reading has become entrenched in international law, creating regulatory capture effects: the doctrine now excludes alternative readings and reserves interpretation authority to the courts that endorse it, which is a form of modest extraction from alternative viewpoints.
 *
 * PERSPECTIVAL GAP:
 *   From the individual rights-bearer's seat, Article 3 is an unambiguous protection: the state cannot execute or torture, full stop. From the state security apparatus's seat, the same constraint is an operational burden: loss of deterrent capacity, higher recidivism, slower response. From the collective security constituency's seat, the constraint is ambiguous: genuine benefit (living in a non-violent state) coupled with real cost (lower security, higher crime risk). The reading itself brackets this gap by privileging the individual rights perspective as foundational—that bracketing is the reading's defining move. The engine should compute this perspectival divergence from the structural data: the same constraint computes as beneficiary at one seat (individual) and payer at another (security apparatus), revealing that what appears as natural law from one angle is extraction from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual rights-bearers sit at the beneficiary end (d near 0.0): they receive the protection from state violence without running the constraint; they have no direct cost. The state security apparatus sits at the target end (d near 1.0): it bears the constraint through prohibited modalities and constrained detention grounds. Collective security constituency sits at the symmetric end (d near 0.5): they accrue diffuse benefits (living in a non-violent state) and distributed costs (slower enforcement, lower deterrence). The positive entitlement partisans are excluded not because they lack power or interest, but because the reading structurally forecloses their alternative; they have no directionality within this story—they represent a counterfactual reading. Due process institutions are the agenda-setter (d near 0.0 as beneficiaries of interpretive authority, though analytically positioned).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if the founding problem (state arbitrariness and violence) had been solved and the constraint persisted for institutional inertia. The founding problem status is 'live,' not 'dead,' because the constraint meets ongoing state violence in jurisdictions that have weakened due process protections (China, Saudi Arabia, many Middle Eastern and sub-Saharan regimes continue capital punishment; torture allegations persist in US detention practices; indefinite detention remains in Israeli military law and other security regimes). The constraint is not a zombie—it is actively contested and actively enforced in signatory jurisdictions. However, mandatrophy resolution enters through the omega variable on whether the negative liberty reading has been overtaken by security-necessity arguments in practice: the gap between the ideal constraint and its actual enforcement suggests partial atrophy of the original vision.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the negative liberty reading a discovery of natural law (individual dignity is foundationally prior to state authority) or a constructed reading favoring individual rights over collective security through interpretive choice?',
    'Comparative constitutional analysis: track whether jurisdictions that do not adopt this reading experience different problems or operate differently. Historical analysis of the UDHR drafting process: did the drafters intend this reading or bracket the substantive question?',
    'If natural law: the reading''s extraction is illusory, a byproduct of discovering pre-existing structure. If constructed: the beneficiaries (individual rights-bearers, constitutional restraint doctrine) are real parties whose interests the reading serves, and the constraint is a Tangled Rope or false summit. The false summit gate (FSM) would fire: mountain + declared beneficiaries → reclassify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether negative liberty is a natural structural fact or a reading choice that benefits certain parties.').

omega_variable(
    security_necessity_versus_rights_absolutism,
    'In genuinely severe security crises (imminent mass violence, terrorism preventing state function), does the narrow procedural constraint legitimately yield to security necessity, or is the constraint absolute?',
    'Empirical: test whether jurisdictions that temporarily suspend due process protections (France after 2015 attacks, Israel''s military detention practices, post-9/11 US detentions) experience measurably better or worse security outcomes than those that maintain the constraint. Normative: clarify whether the reading permits emergency deviation or asserts absolute prohibition.',
    'If the constraint yields to security necessity, it is Scaffold or Tangled Rope, not Mountain—it is a coordinated arrangement with built-in override conditions. If absolute, the reading''s extractiveness from security apparatus is even higher, and the false summit candidate becomes stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_necessity_versus_rights_absolutism, empirical, 'Whether Article 3''s prohibition on arbitrary deprivation is absolute or permits emergency override.').

omega_variable(
    enforcement_capacity_versus_will,
    'The constraint''s low enforcement cost (suppression = 0.19) assumes due process institutions are willing and capable of checking state power. Is the constraint''s persistence a function of genuine legal authority (institutions can bind the state) or performative consensus (state actors comply when it suits them and violate when it doesn''t)?',
    'Comparative institutional analysis: jurisdictions where courts successfully enjoin state violence (Western Europe, Commonwealth) vs. jurisdictions where state actors routinely ignore due process findings (many Global South regimes). Post-suppression trajectory analysis: if a state rejects the constraint, does the constraint persist through international pressure, or does the constraint''s authority collapse?',
    'If persistence depends on state will, the suppression metric understates the constraint''s fragility and the true cost to security apparatus (they choose to comply because of other incentives, not because they are bound). If courts can genuinely bind the state, the constraint is more robust than the theory suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_versus_will, empirical, 'Whether the constraint''s low suppression reflects genuine institutional authority or performative state compliance.').

omega_variable(
    beneficiary_identity_fusion,
    'Have individual rights-bearers and constitutional institutions become so fused with the negative liberty reading that the reading''s revision would threaten their identity (professional identity for lawyers, ideological identity for human rights advocates)?',
    'Qualitative: interviews with legal professionals and human rights advocates about whether they view due process protection as extrinsic policy or intrinsic to professional/moral identity. Behavioral: measure resistance to security-necessity arguments that propose narrowing due process—is the resistance principled or performative?',
    'If identity-fused, the constraint''s suppression cost is higher than 0.19 suggests (the fused parties will actively defend it). Exit for those parties becomes identity_locked rather than mobile, which changes their directionality. The false summit candidate depends on whether this fusion is evidence of natural law or proof of constructed beneficiary capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_fusion, empirical, 'Whether the negative liberty reading has fused with the identity of its beneficiaries, making revision identity-threatening.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(udhr_tr_t1965, udhr_article_3__negative_liberty_reading, theater_ratio, 1965, 0.09).
narrative_ontology:measurement(udhr_tr_t1980, udhr_article_3__negative_liberty_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(udhr_tr_t2000, udhr_article_3__negative_liberty_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement(udhr_tr_t2015, udhr_article_3__negative_liberty_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(udhr_tr_t2026, udhr_article_3__negative_liberty_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_be_t1965, udhr_article_3__negative_liberty_reading, base_extractiveness, 1965, 0.18).
narrative_ontology:measurement(udhr_be_t1980, udhr_article_3__negative_liberty_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(udhr_be_t2000, udhr_article_3__negative_liberty_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(udhr_be_t2015, udhr_article_3__negative_liberty_reading, base_extractiveness, 2015, 0.27).
narrative_ontology:measurement(udhr_be_t2026, udhr_article_3__negative_liberty_reading, base_extractiveness, 2026, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.12).
narrative_ontology:measurement(udhr_su_t1965, udhr_article_3__negative_liberty_reading, suppression_requirement, 1965, 0.14).
narrative_ontology:measurement(udhr_su_t1980, udhr_article_3__negative_liberty_reading, suppression_requirement, 1980, 0.16).
narrative_ontology:measurement(udhr_su_t2000, udhr_article_3__negative_liberty_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(udhr_su_t2015, udhr_article_3__negative_liberty_reading, suppression_requirement, 2015, 0.19).
narrative_ontology:measurement(udhr_su_t2026, udhr_article_3__negative_liberty_reading, suppression_requirement, 2026, 0.19).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Article 3 of the UDHR is a contested kernel with three primary readings: negative liberty (this story), positive entitlement, and procedural hybrid. Each reading instantiates a different constraint with different beneficiaries, victims, and extractiveness. The negative liberty reading anchors Article 3 in freedom from state violence; the positive entitlement reading anchors it in affirmative state provision; the procedural hybrid reading focuses on due process safeguards without resolving the substantive contest. These are NOT three angles on one constraint—they have structurally different ε values and different victim/beneficiary structures. The negative liberty reading forecloses the positive entitlement reading within any single framework but coexists with the procedural hybrid reading across different interpretive communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
