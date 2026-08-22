% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Divine Legitimacy (Household Ritual)
 *   domain: religious/political economy
 *
 * SUMMARY:
 *   In Egyptian society, divine legitimacy — the claim that gods authorize
 *   and sustain proper order — flowed through multiple institutional
 *   channels. The folk syncretistic reading treats divine legitimacy as
 *   something that flows THROUGH household and village ritual practice:
 *   multiple deities are invoked pragmatically based on need (crops, health,
 *   protection, healing), not through a unified theological doctrine. No
 *   single priesthood or pharaoh mediates this legitimacy; instead,
 *   households and villages maintain it themselves through local ritual
 *   coordination. This reading is historically persistent — household
 *   shrines, village calendars, and pragmatic multi-deity invocation are
 *   archaeologically visible throughout Egyptian history — yet it sits in
 *   tension with official priestly theology (which claims Amun-Ra as chief
 *   deity) and with periodic pharaonic attempts at religious reform (notably
 *   Atenism, which demanded exclusive monotheism). The constraint story
 *   models folk syncretism as a Rope: it solves genuine coordination problems
 *   (labor synchrony, dispute resolution, community cohesion) through a
 *   distributed, locally-managed mechanism that benefits households and
 *   villages. The extraction it requires is moderate: household participation
 *   in the ritual economy and some labor/resource contribution to collective
 *   projects. The constraint is genuinely contested — reforming priesthoods
 *   and pharaohs have repeatedly attacked it — but it persists because it
 *   solves problems that centralized authority cannot reach, and because
 *   participation in it is identity-constitutive for villages.
 *
 * KEY AGENTS:
 *   - household_ritual_practitioners — age-based roles (elders, women, men) conducting daily/seasonal rituals; local authority and efficacy belief; identity-locked participation
 *   - village_elders — coordinators of ritual calendar and dispute resolution; organized power; beneficiaries from authority; constrained exit
 *   - pharaonic_priesthood — claims supreme theological authority; institutional power; treats folk practice as subordinate but tolerates it; observer role with institutional constraints
 *   - pharaoh — claims unique divine mediation; powerful but distant; folk reading treats claim as theatrical; analyst seat
 *   - monotheistic reformers — historically excluded from village-level conversation; attempt suppression when gaining state power; explicitly foreclose syncretism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.38).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.25).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Divine Legitimacy (Household Ritual)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/political economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '9a0cad61-6ac3-4a2c-8014-9e237cdbcc13').
narrative_ontology:cs_kernel_codification('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', distributed).
narrative_ontology:cs_authority_grounding('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', practice).
narrative_ontology:cs_interpretation_layer_present('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13').
narrative_ontology:cs_reading_relation('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', foundational, divine_legitimacy_distributed_and_household_mediated).
narrative_ontology:cs_axiom_status(divine_legitimacy_distributed_and_household_mediated, holdable).
narrative_ontology:cs_axiom_grounding('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', divine_legitimacy_distributed_and_household_mediated, deontological).
narrative_ontology:cs_axiom('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', secondary, local_ritual_community_authority_superior_to_institutional_claim).
narrative_ontology:cs_axiom_status(local_ritual_community_authority_superior_to_institutional_claim, holdable).
narrative_ontology:cs_axiom_grounding('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', local_ritual_community_authority_superior_to_institutional_claim, instrumental).
narrative_ontology:cs_reference_frame('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', distributed_household_divine_legitimacy).
narrative_ontology:cs_drift_state('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', late_pharaonic_centralization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9a0cad61-6ac3-4a2c-8014-9e237cdbcc13', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_ritual_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_community_cohesion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct daily and seasonal rituals incorporating multiple deities (household gods, Nile spirits, ancestor veneration, pragmatically invoked greater deities). The practice itself IS the legitimacy claim — no doctrinal authority mediates it. Practitioners benefit from collective efficacy belief (crops, health, community stability) and from the social cohesion the rituals maintain. Their role is both agent and beneficiary: they perform the rituals and collect the social/spiritual goods.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_ritual_practitioners, agenda_setter,
    moderate, generational, identity_locked, local).

% Coordinate the ritual calendar and adjudicate disputes over proper practice. They hold no formal authority outside the village but are the de facto interpreters of what 'correct' ritual looks like locally. They benefit from the authority granted them by consensus and from the social stability the coordinated rituals produce. Their exit is constrained: stepping down from elder status requires explicit community recognition and typically occurs only through death or incapacity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_elders, beneficiary).

% Maintains official temple theology and canonical deity hierarchy (Amun-Ra, state deities). The folk practice is largely beneath their attention and doctrinal authority; they claim supreme interpretive authority over divine legitimacy but cannot fully control or revise village-level ritual. They tolerate folk practice as long as it does not openly challenge the pharaonic cult or fail to contribute temple taxes/labor. Their constraints are institutional: they are bound by the role they occupy and cannot freely abandon the claim to authority even if it exceeds their reach.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_priesthood, observer,
    institutional, generational, constrained, national).

% Claims to be the living incarnation of divine will and mediator between cosmos and Egypt. The folk reading treats this claim as institutional theater — the pharaoh is a distant elite whose authority is separate from the legitimacy of household practice. The pharaoh's enforcement capacity over village ritual is low; actual leverage comes through tax demands and priesthood mediation, not direct coercion. The pharaoh is trapped by their own role: they cannot step down from the claim to divine mediation without losing legitimacy at court, even though villagers ignore the claim.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaoh, observer,
    powerful, generational, trapped, national).

% Advocate for exclusive devotion to a single deity (historically, Aten under Akhenaten; in other periods, reform movements toward monotheism). Their doctrine explicitly forecloses the folk syncretistic practice as heretical. They are excluded from the village conversation — their authority does not reach the household level — but when they do gain state power, they attempt to suppress the folk practice and impose exclusive orthodoxy. They are trapped by their own doctrinal claim: they cannot acknowledge the validity of folk practice without undermining the monotheistic principle.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, monotheistic_reformers, excluded,
    powerful, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__folk_syncretistic_reading, diffuse).
narrative_ontology:fixing_cost_class(divine_legitimacy_substrate__folk_syncretistic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes household ritual practice across the village through a shared calendar and multi-deity invocation, enabling collective labor (irrigation, harvest, defense) and dispute resolution. Each household maintains autonomy over its own deities while participating in village-level synchrony through recognized seasonal rituals.
% TRANSFER_FUNCTION: Moves household labor and resource contribution toward community synchrony (participation in collective projects) and toward petty redistribution during ritual feasts. Spiritual authority flows FROM the people TO themselves (collective efficacy belief) rather than downward from a distant priesthood; the people are both agents and beneficiaries.
% ABSENT_VOICES: State-level monotheistic reformers would object that the practice is theologically incoherent and that exclusive devotion to a single god is the only legitimate path. Pharaonic priesthood would assert their authority is supreme and the folk practice should be subordinate to official theology. Their exclusion from the village conversation is structural — they lack enforcement reach at the household level — but when they do gain power, the constraint is directly attacked.
% DISAPPEARANCE_RATIONALE: If household/village syncretistic ritual vanished, village labor coordination would collapse or be reorganized through non-ritual mechanisms (formal conscription, external authority). Dispute resolution would fracture without the shared legitimacy substrate. The spiritual substrate of community identity would need reconstruction through either individual household religion or imposed state orthodoxy.
% FOUNDING_PROBLEM: Pre-literate and early-literate villages needed coordination mechanisms for collective labor (irrigation, harvest) and conflict resolution that did not depend on literacy, external authority, or centralized administration. Multi-deity household ritual practice provided a distributed, locally adaptable legitimacy substrate: gods could be invoked for practical efficacy (water, healing, protection) without requiring unanimous theological doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Archaeologists and anthropologists document the persistence of household-level multi-deity practice across Egyptian history despite official temple dogma and pharaonic claims to exclusive divine mediation. Village-level administrative texts (letters, records, settlement patterns) show coordination through ritual obligation and seasonal timing. The problem remains live wherever centralized authority is weak or distant — households need legitimacy substrates they control, and multi-deity pragmatism solves that where monolatry or centralized priesthood cannot reach.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at end) is moderate because the constraint moves household labor and some resources toward collective projects and ritual feasts, but benefits are clearly internalized within the village (labor coordination, dispute resolution, community identity). This is genuine coordination, not predatory extraction. Suppression is low (0.25) because the constraint persists without active state enforcement; indeed, villagers maintain it against periodic state pressure, suggesting it is sustained by genuine preference and identity-commitment rather than coercion. Theater is very low (0.12) because the rituals serve identifiable practical functions (coordination, healing, protection-belief) rather than performative maintenance of a function that has atrophied. The measurement series traces a slight rise in extractiveness (0.32→0.38 over 50 time units) reflecting gradual intensification of resource demands as villages grew more densely settled and coordination burdens increased, plateauing then stabilizing as administrative overhead reached a steady state. Theater remains flat and low, confirming the practice is functionally maintained, not theatrically preserved. The claim of Rope is grounded in the coordination function: the syncretistic practice solves multiple collective-action problems that would be harder to solve through centralized authority or through individual household isolation.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic priesthood and pharaoh perceive this constraint from an observer/institutional position that claims authority they do not actually exercise at the village level. They author narratives of supreme priestly wisdom and pharaonic divine mediation, but villagers experience those claims as distant institutional theater. From the practitioner seat, the legitimacy is LOCAL and collective; from the institutional seat, legitimacy flows downward from the state. The engine computes these divergent seats from the structural data: practitioners with moderate power and identity-locked exit derive directionality near 0.5 (symmetric or beneficiary); priesthood and pharaoh with institutional power and constrained/trapped exit derive directionality nearer 1.0 (targets of the system, or external observers constrained by their own institutional logic). The gap between 'supreme theological authority' (claimed) and 'observer with limited reach' (structural) is the analytic payload.
 *
 * DIRECTIONALITY LOGIC:
 *   Household practitioners are beneficiaries and agenda-setters: they define the practice, collect the coordination and spiritual goods, and maintain it through identity-commitment. Their directionality is low (beneficiary end, d~0.25). Village elders are also beneficiaries/agenda-setters but with organized power; their directionality is also low (d~0.30). The pharaonic priesthood claims authority but cannot enforce it at the village level; they are structurally observers with constrained exit (they cannot control the villages without destroying the practice's legitimacy, but they are constrained by their own institutional role to assert authority). Their directionality is high (d~0.80: targets of the constraint, in a sense — constrained to a role they cannot fully execute). The pharaoh is similarly institutional and trapped; directionality high (d~0.85). Monotheistic reformers are explicitly excluded; when they do gain power, they become payers (bearing the cost of suppressing the constraint), so their directionality among excluded seats is computed from trapped+powerful status (d~0.70, near-full target). The practice treats these institutional seats as obstacles rather than sources of legitimacy — they are observed, not coordinated with.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: villages continue to need coordination mechanisms independent of centralized authority, and folk syncretistic practice continues to provide them. The disappearance verdict is world_rearranges: if the practice vanished, village labor coordination and dispute resolution would either collapse or reorganize under state/priestly authority. The constraint is not mandatrophied — it remains functionally necessary and actively performed. However, there is a dynamic tension: as state capacity increases (later periods), the constraint becomes increasingly contested and faces more active suppression. The measurement series shows extractiveness rising slightly as coordination demands intensify, but not theater or suppression — this pattern is consistent with a Rope under mild pressure but not yet degrading into either Piton (if suppression rose sharply, theater would rise with it) or Snare (if extractiveness spiked, victims would need naming, and we would see organized resistance metrics). The mandate persists: villages continue to validate the founding problem even as state authorities dispute it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretism_vs_incoherence_reading,
    'Is folk syncretistic practice a coherent theological position that pragmatically accommodates multiple divine agents, or is it theologically incoherent opportunism masking the absence of genuine belief?',
    'Ethnographic and textual analysis of household ritual reasoning: do practitioners articulate a principle for when/why multiple deities are invoked together, or are invocations random? Do ritual specialists (healers, diviners) teach a transmissible theology, or is it tacit and variable?',
    'If coherent, the constraint is a Rope with a genuine coordination function grounded in folk epistemology. If incoherent, it may be better classified as a Piton — a residual practice maintained theatrically by inertia without real theological substance. This reading assumes coherence; overridden status of that axiom would demote the type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretism_vs_incoherence_reading, conceptual, 'Whether folk syncretism is a coherent theological system or theatrical residue.').

omega_variable(
    identity_lock_mechanism,
    'Is the exit_options: identity_locked designation for practitioners justified? Is participation in household/village syncretistic ritual identity-constitutive such that exit is cognitively impossible even if material costs rose, or is it merely culturally expected (exit_options: constrained)?',
    'Historical evidence from periods of state pressure (Atenist suppression, later Christianization attempts): do households abandon the practice when the material cost of persistence rises, or do they maintain it despite severe punishment?',
    'If identity-locked, suppression is internalized and persists across generations even after institutional pressure lifts; effective suppression of this constraint is higher than measured. If merely constrained, household practice could shift if external pressure reversed. This determines whether the constraint exhibits post-exit persistence (internalized suppression marker) or post-exit recovery (structural suppression only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether ritual participation is identity-locked or merely constrained by social pressure.').

omega_variable(
    pharaonic_tolerance_boundary,
    'What is the actual boundary of pharaonic and priestly tolerance for folk syncretistic practice? Is the measured suppression (0.25) because they actively permit it, or because they lack enforcement reach?',
    'Archival evidence from periods of active state religious reform (Atenist period, later priestly centralization): did the state suppress folk practice when it could, or did it tolerate it as a matter of policy?',
    'If active tolerance: the constraint benefits from institutional non-interference and suppression is genuinely low. If passive toleration (inability to enforce): suppression may be held artificially low by geography/logistics and could spike if state capacity increased. This determines whether the measured suppression reflects true consensus or merely logistical barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaonic_tolerance_boundary, empirical, 'Whether pharaonic suppression is low by design or by incapacity.').

omega_variable(
    kernel_reading_alternative,
    'This constraint instantiates the folk_syncretistic_reading of the divine_legitimacy_substrate kernel. How does this reading''s ε and structural relationship to authority differ from the sibling amun_polytheistic_reading (priestly canonical interpretation) and atenist_monotheistic_reading (pharaonic revelation)?',
    'Compare the three reading stories: folk reading treats legitimacy as distributed and locally-maintained (low extractiveness, diffuse authority); amun reading treats it as vertically organized through priesthood (higher extractiveness, concentrated authority); aten reading forecloses both by claiming exclusive divine revelation (potentially highest extractiveness via state enforcement, highest suppression). The three readings represent alternative instantiations of the same kernel — the claim that divine legitimacy is a real and necessary feature of governance — under different authority structures.',
    'This is a conceptual/framing omega: the three readings are structurally distinct constraints with different ε values, beneficiary structures, and authority grounds. The folk reading''s relatively low extractiveness and distributed authority differentiate it from the concentrated extraction in the priestly and pharaonic readings. This omega documents that the kernel itself (divine legitimacy matters) is shared, but the FORM legitimacy takes (distributed vs. hierarchical vs. exclusive) produces radically different types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative, conceptual, 'This constraint as one reading of the divine_legitimacy_substrate kernel, structurally distinct from sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(divi_tr_t30, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(divi_be_t30, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate__atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel is instantiated by three structurally distinct constraint stories, each representing a different reading of how divine legitimacy flows through Egyptian society. The folk_syncretistic_reading (this story) treats legitimacy as distributed through household/village practice; the amun_polytheistic_reading treats it as hierarchical through priestly mediation; the atenist_monotheistic_reading treats it as monopolistic through pharaonic revelation. Each reading has its own ε value, beneficiary/victim structure, authority grounding, and computational type. They are not alternative measurements of the same constraint — they are different constraints instantiated from the same kernel by different interpretive communities. The three stories are linked via network.affects_constraints to show the kernel family relationship and the conceptual interdependencies: priestly theology (Amun reading) influences but does not foreclose folk practice (this reading); pharaonic monotheism (Aten reading) forecloses both by claiming exclusive truth; folk practice persists because it solves problems at the village scale that the other readings cannot reach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
