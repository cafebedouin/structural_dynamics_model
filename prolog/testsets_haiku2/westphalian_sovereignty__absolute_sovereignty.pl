% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty Doctrine
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The absolute sovereignty reading instantiates the doctrine that states
 *   possess unconditional authority over domestic affairs and that external
 *   interference—humanitarian, military, or legal—is categorically
 *   illegitimate. Under this reading, the principle of non-interference is
 *   foundational to international law and cannot be overridden by claims
 *   about human rights violations or humanitarian need. This reading benefits
 *   authoritarian and repressive regimes, which invoke it to block
 *   intervention and accountability mechanisms. Populations under these
 *   regimes are victimized by the doctrine's operation—their access to
 *   external remedy is barred by the same rule that protects state authority.
 *   This is a kernel reading of contested Westphalian sovereignty; it
 *   coexists with conditional_sovereignty (which permits intervention when
 *   systematic violations occur) and graduated_sovereignty (which conditions
 *   state authority on capacity and legitimacy). The constraint is AUTHORED
 *   as tangled_rope: genuine coordination function (the meta-rule preventing
 *   intervention chaos) paired with asymmetric extraction (shielding
 *   repressive regimes and victimizing internal populations). The measurement
 *   series shows extraction accumulating from t=0 to t=30 and then
 *   plateauing—the constraint tightens as authoritarian regimes consolidate
 *   the doctrine and democratic states increasingly practice selective
 *   interventions while maintaining absolute sovereignty rhetoric.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes (institutional power, beneficiary): invoke sovereignty to block accountability
 *   - State apparatus elites (institutional power, agenda-setter): codify and defend the doctrine through diplomacy
 *   - Oppressed domestic populations (powerless, payer): victimized by the shield the doctrine provides
 *   - Minority groups (powerless, identity-locked, payer/beneficiary): structurally bound to territory, alternately protected and victimized
 *   - Western democracies (powerful, beneficiary/observer): benefit from sovereignty when convenient, practice selective intervention
 *   - UN P5 (institutional power, agenda-setter): control enforcement through veto; define which violations trigger intervention
 *   - Global South states (powerful/constrained, beneficiary): invoke doctrine against Western pressure
 *   - International human rights bodies (organized, excluded): barred from enforcement by the doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.68).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'a7e2d72a-6cb2-4160-8696-10c56ffb495c').
narrative_ontology:cs_kernel_codification('a7e2d72a-6cb2-4160-8696-10c56ffb495c', fixed_text).
narrative_ontology:cs_authority_grounding('a7e2d72a-6cb2-4160-8696-10c56ffb495c', extraction).
narrative_ontology:cs_interpretation_layer_present('a7e2d72a-6cb2-4160-8696-10c56ffb495c').
narrative_ontology:cs_reading_relation('a7e2d72a-6cb2-4160-8696-10c56ffb495c', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('a7e2d72a-6cb2-4160-8696-10c56ffb495c', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('a7e2d72a-6cb2-4160-8696-10c56ffb495c', foundational, unconditional_state_authority).
narrative_ontology:cs_axiom_status(unconditional_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('a7e2d72a-6cb2-4160-8696-10c56ffb495c', unconditional_state_authority, deontological).
narrative_ontology:cs_axiom('a7e2d72a-6cb2-4160-8696-10c56ffb495c', foundational, non_interference_categorical).
narrative_ontology:cs_axiom_status(non_interference_categorical, holdable).
narrative_ontology:cs_axiom_grounding('a7e2d72a-6cb2-4160-8696-10c56ffb495c', non_interference_categorical, conventional).
narrative_ontology:cs_reference_frame('a7e2d72a-6cb2-4160-8696-10c56ffb495c', unconditional_state_authority).
narrative_ontology:cs_drift_state('a7e2d72a-6cb2-4160-8696-10c56ffb495c', contemporary_humanitarian_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a7e2d72a-6cb2-4160-8696-10c56ffb495c', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, state_apparatus_elites).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, oppressed_domestic_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, minority_groups_within_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, minority_groups_within_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, western_democracies).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, global_south_non_aligned_states).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, state_non_interference_doctrine).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, territorial_integrity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritarian and semi-authoritarian states benefit from the non-interference shield—the doctrine legitimizes their claim that internal repression is off-limits to external critique, sanctions, or intervention. They invoke sovereignty to block humanitarian intervention, refugee advocacy, and cross-border accountability mechanisms. Their power derives from controlling the legal apparatus and shaping the discourse around state legitimacy.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, global).

% State-level foreign ministries, delegations to the UN, and diplomatic corps codify and defend the absolute sovereignty doctrine through formal statements, treaty negotiations, and blocking attempts at humanitarian intervention. They set the agenda by voting in international forums, negotiating treaties, and selectively invoking sovereignty when it protects their interests.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, state_apparatus_elites, agenda_setter,
    institutional, biographical, analytical, global).

% Populations under repressive regimes bear the costs of the absolute sovereignty doctrine: they experience systematic human rights violations without remedy because the doctrine categorizes the regime's treatment of them as internal, off-limits to external intervention. Their exit options are severely constrained—they cannot vote out the regime, cannot appeal to international law, and face violence if they attempt to organize resistance.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, oppressed_domestic_populations, payer,
    powerless, biographical, trapped, local).

% Ethnic, religious, and political minorities in states where sovereignty shields the ruling group face targeted persecution. They benefit from the principle when their minority group holds state power; they are victimized when the majority does. Their identity is territorially bound—they cannot simply leave without severing fundamental ties. Exit via emigration is available to some individuals but not to the group as a collective.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, minority_groups_within_states, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, minority_groups_within_states, beneficiary).

% Democratic states benefit from absolute sovereignty doctrine when it prevents other powerful states from interfering in their affairs; they simultaneously invoke humanitarian concerns to justify selective interventions in weaker states (inconsistency managed through rhetorical distinction between 'sovereignty' and 'responsibility to protect'). They have the military capacity and diplomatic leverage to intervene selectively while invoking sovereignty when convenient.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, western_democracies, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, western_democracies, observer).

% International human rights courts, treaty bodies, and NGOs are structurally barred from enforcing accountability across borders when the absolute sovereignty doctrine is strictly applied. They can document violations and issue reports, but lack enforcement mechanisms. States blocking intervention use absolute sovereignty as their shield; these bodies would argue for jurisdiction and enforcement authority if permitted.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_bodies, excluded,
    organized, generational, constrained, global).

% P5 states interpret sovereignty doctrine selectively, voting to protect allies while authorizing interventions against adversaries. The veto power allows them to define which violations trigger intervention and which fall under sovereignty. Their enforcement power is massive—they control military capacity, economic sanctions authority, and formal intervention authorization.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, un_security_council_permanent_members, agenda_setter,
    institutional, generational, analytical, global).

% States outside the Western/P5 alignment benefit from absolute sovereignty doctrine as protection against great-power intervention. They invoke it against sanctions, conditional aid, and external pressure on governance. Their power is structural (numbers in the General Assembly) and strategic (control over resources, geography) but constrained by economic dependency and lack of veto authority in the Security Council.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, global_south_non_aligned_states, beneficiary,
    powerful, generational, constrained, global).

% Legal theorists and international relations scholars analyze and debate the doctrine's empirical consequences, legitimacy grounding, and alternative framings. They produce competing interpretations and evidence; the constraint's legitimacy depends partly on their scholarly consensus or contestation. They have no direct enforcement power but shape how the constraint is understood.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rule-based international system where territorial states are the primary units of authority and non-interference is the meta-rule: reduces great-power conflicts over spheres of influence and provides a framework (however imperfect) for negotiation instead of continual war over which power can intervene where. Solves the collective action problem of how to prevent universal intervention chaos.
% TRANSFER_FUNCTION: Transfers legitimacy and enforcement immunity from the international community to state governments—specifically, from external accountability to internal authority. Oppressed populations lose access to external remedy; authoritarian elites gain a shield. Repressive state apparatus gains the authority to act internally without external legal challenge.
% ABSENT_VOICES: Populations under repressive regimes are de jure excluded from international advocacy—they have no seat at the UN, no official standing in treaty negotiation. Stateless persons, refugee communities, and internally displaced persons are also structurally absent. Nongovernmental human rights organizations participate only as observers without voting authority. Internal dissidents and opposition movements are present only if the state permits their representatives (which authoritarian states prevent).
% DISAPPEARANCE_RATIONALE: If absolute sovereignty doctrine disappeared and were replaced with conditional sovereignty or humanitarian intervention authority, the distribution of state power would shift dramatically: authoritarian regimes would lose their protective shield, democratic states would gain grounds for intervention, international courts would gain enforcement authority over internal matters. The state-centric system itself might fragment under pressure from supranational authority. Regimes currently shielded by sovereignty would reorganize their governance to avoid international intervention or would face forced regime change.
% FOUNDING_PROBLEM: The Westphalian settlement (1648) was built to solve the religious wars of early modern Europe by establishing that states, not supranational religious authority, should control internal governance. The founding problem was preventing external powers (particularly the Vatican and rival empires) from intervening to enforce religious uniformity or dynastic claims across borders.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars and historians acknowledge the founding problem is resolved—no state is threatened by external religious imposition or dynastic intervention in the modern era. However, state governments (particularly those with poor human rights records) invoke the doctrine as if the founding problem is still live. Democratic governments selectively invoke it when convenient while claiming humanitarian exceptions. The dead-but-invoked status is corroborated by historians of the Westphalian system and by the gap between stated doctrine and actual practice documented in case studies of humanitarian intervention (Kosovo, Libya, Syria—inconsistently applied).
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52 at t=30, rising from 0.38 at t=0) because the doctrine's primary function is to shield state authority from external accountability—this extraction increases as regimes become more repressive and the protection more valuable. The measurement series shows steady accumulation over the first 30 time points (corresponding to roughly 1648–2000), then plateaus at 0.52 as the doctrine saturates—further tightening is constrained by the rising power of humanitarian rhetoric and the increasing costs of maintaining absolute sovereignty claims in the face of documented atrocities. Suppression is high (0.68) and rising because maintaining the absolute sovereignty doctrine requires actively excluding international courts, preventing intervention, and blocking enforcement mechanisms—the doctrine's persistence depends on suppressing countervailing institutions. Theater rises from 0.25 to 0.42 because the doctrine's functional coordination (preventing intervention chaos) decoupled from its actual operation (protecting repressive regimes) as democratic states began practicing selective intervention while maintaining absolute sovereignty language. The theater increase reflects growing performative defense of the doctrine (e.g., states invoke sovereignty in the General Assembly while simultaneously authorizing interventions through NATO or ad hoc coalitions). The temporal pattern models the constraint's lifecycle: it began as genuine coordination (post-Westphalian, t=0), accumulated extractive capacity as authoritarian regimes consolidated (t=0–30), and then entered a high-theater phase as its contradictions became undeniable (t=30–40).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (authoritarian regimes), this constraint is experienced as genuine coordination—a rule-based system that prevents powerful states from imposing their will on weaker ones, thereby protecting their authority and making international relations predictable. From the victim seats (oppressed populations), the same rule is experienced as enforced victimization—a doctrine explicitly invoked to deny them remedy and block external protection. The engine should compute a tangled_rope classification for the beneficiary seat and a snare classification for the victim seats, reflecting this asymmetry. The Western democracies seat computes differently again—they experience the doctrine as flexible coordination, applicable strictly when it serves their interests and violated strategically when they deem intervention justified. The agenda-setter seats (state apparatus, P5) compute the constraint as an enforcement mechanism they control, neither purely extraction nor purely coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (authoritarian regimes, state elites, Western democracies selectively) have d near the beneficiary end (0.1–0.3): they collect immunity from accountability and authority over internal governance. The victimized seats (oppressed populations, minority groups) have d near the target end (0.85–0.95): they experience suppression and cannot exit without severing identity or facing violence. The Western democracies seat is complex (d ≈ 0.4–0.5): they benefit from the doctrine as a shield when it suits them but also employ its violation to justify interventions they control—they are positioned symmetrically between beneficiary and agenda-setter, neither pure extractors nor pure targets. The UN P5 and state apparatus seats sit at agenda-setter d (0.3–0.5): they set and enforce the rules but are not the primary extractors (they benefit from the enforcement authority, not the protection itself, except when their own sovereignty is at stake). The Global South states benefit defensively (d ≈ 0.25–0.35): they use sovereignty to block Western pressure but lack the power to redefine it unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing religious and dynastic intervention chaos in early modern Europe) is unambiguously dead. No state faces the Westphalian threat anymore. However, the doctrine persists with high theater—states invoke absolute sovereignty and non-interference at the UN while simultaneously practicing selective intervention. The mandatrophy is visible in the gap between founding_problem_status=dead and disappearance_verdict=world_rearranges: if the doctrine disappeared, the world would rearrange because it now shelters repressive regimes rather than solving the founding coordination problem. The constraint has shifted from rope (genuine coordination) to tangled_rope (coordination + extraction) to increasingly tangled/snare (mostly extraction, theater-sustained). The theater_ratio's rise from 0.25 to 0.42 models this drift—the constraint is increasingly maintained performatively (states must declare commitment to sovereignty even as they violate it) rather than functionally (the doctrine actually preventing intervention chaos). Remedies would require either (a) explicit conditional_sovereignty doctrine change (authorizing intervention for rights violations) or (b) graduated_sovereignty restructuring (making authority dependent on legitimacy). Both are upstream of this reading; they would foreclose or substantially influence this constraint if adopted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is absolute sovereignty a natural fact about how states must function (sovereignty-as-necessity) or a constructed doctrine that benefits specific parties and persists because they defend it?',
    'Historical comparison: examine pre-Westphalian international systems to test whether sovereignty-equivalent arrangements emerged independently or required deliberate construction. If they did not emerge and must be actively maintained, the constraint is constructed not natural.',
    'If natural-law status can be falsified, the constraint reclassifies from mountain toward snare/tangled_rope. If it is genuinely natural, the high extraction values are consistent with a mountain where some actors happen to benefit incidentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, empirical, 'Whether absolute sovereignty is a necessary feature of international systems or a constructed doctrine.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of international human rights bodies and intervention mechanisms structural (laws and treaties barring enforcement) or internalized (states have accepted the legitimacy of non-interference doctrine)?',
    'Behavioral test: remove the legal barriers and observe whether states voluntarily maintain sovereignty norms or immediately exercise intervention authority. If they maintain norms, suppression is internalized; if they immediately intervene, suppression is structural.',
    'If internalized, the constraint''s power persists even after legal change, making remedies harder (cultural shift required). If structural, legal reform (e.g., conditional_sovereignty treaty) could break the constraint relatively quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of intervention mechanisms is enforced externally or internalized as doctrine.').

omega_variable(
    beneficiary_disagreement_on_extractiveness,
    'Do authoritarian regimes consciously recognize that they benefit from the non-interference shield, or do they genuinely believe absolute sovereignty is a neutral principle?',
    'Discourse analysis: examine rhetorical patterns in how states invoke sovereignty. Conscious beneficiaries deploy the doctrine strategically in different contexts; true believers invoke it universally. Compare voting patterns and rhetoric.',
    'Conscious beneficiaries suggest deliberate rent-seeking (snare dynamics); true believers suggest the doctrine''s legitimacy is genuine and extraction is incidental. Different dynamics for remediation and political change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_disagreement_on_extractiveness, conceptual, 'Whether beneficiaries recognize the constraint''s extractive function or genuinely believe in its legitimacy.').

omega_variable(
    reading_foreclosure_test,
    'Do the axioms of absolute_sovereignty logically foreclose conditional_sovereignty or merely coexist as rival interpretations?',
    'Formal analysis: test whether a single state could simultaneously hold ''non-interference is categorical'' and ''intervention is legitimate when rights are violated'' without contradiction. If it can (by distinguishing contexts or authority levels), they coexist; if it cannot, foreclosure holds.',
    'If forecloses: the readings are logically opposed; choosing one forecloses the other; institutional pressure toward consolidation. If coexists: readings remain live options; institutional compromise is possible; longer-term competition likely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Logical relationship between absolute_sovereignty and conditional_sovereignty readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(west_tr_t0, observed).
narrative_ontology:measurement(west_tr_t5, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(west_tr_t5, observed).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(west_tr_t10, observed).
narrative_ontology:measurement(west_tr_t15, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(west_tr_t15, observed).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(west_tr_t20, observed).
narrative_ontology:measurement(west_tr_t25, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(west_tr_t25, observed).
narrative_ontology:measurement(west_tr_t30, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(west_tr_t30, observed).
narrative_ontology:measurement(west_tr_t35, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(west_tr_t35, projected).
narrative_ontology:measurement(west_tr_t40, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(west_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(west_be_t0, observed).
narrative_ontology:measurement(west_be_t5, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(west_be_t5, observed).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(west_be_t10, observed).
narrative_ontology:measurement(west_be_t15, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(west_be_t15, observed).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(west_be_t20, observed).
narrative_ontology:measurement(west_be_t25, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 25, 0.51).
narrative_ontology:measurement_basis(west_be_t25, observed).
narrative_ontology:measurement(west_be_t30, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(west_be_t30, observed).
narrative_ontology:measurement(west_be_t35, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 35, 0.52).
narrative_ontology:measurement_basis(west_be_t35, projected).
narrative_ontology:measurement(west_be_t40, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 40, 0.52).
narrative_ontology:measurement_basis(west_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(west_su_t0, observed).
narrative_ontology:measurement(west_su_t5, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(west_su_t5, observed).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(west_su_t10, observed).
narrative_ontology:measurement(west_su_t15, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(west_su_t15, observed).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(west_su_t20, observed).
narrative_ontology:measurement(west_su_t25, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(west_su_t25, observed).
narrative_ontology:measurement(west_su_t30, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(west_su_t30, observed).
narrative_ontology:measurement(west_su_t35, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 35, 0.68).
narrative_ontology:measurement_basis(west_su_t35, projected).
narrative_ontology:measurement(west_su_t40, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(west_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, international_court_jurisdiction).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, cross_border_enforcement).

% DUAL FORMULATION NOTE:
% The westphalian_sovereignty kernel decomposes into three reading-specific constraints: absolute_sovereignty (this file, ε≈0.52, tangled_rope), conditional_sovereignty (ε≈0.65, snare/tangled_rope, victims expanded), and graduated_sovereignty (ε≈0.58, tangled_rope, beneficiaries stratified). Each reading instantiates different beneficiary/victim structures and ε values from the same kernel. The constraint_id naming convention (constraint__reading) preserves reading provenance. All three are linked via network.affects_constraints to enable tracking of reading competition and foreclosure dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
