% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__symbol_survival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__symbol_survival_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__symbol_survival_reading
 *   human_readable: Ritual Form Preservation and Identity Boundary Maintenance (Symbol-Survival Reading)
 *   domain: religious/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models Jewish identity preservation through ritual
 *   symbolic practice post-catastrophe, instantiating the symbol-survival
 *   reading of the contested kernel 'catastrophe_memory_survival.' The
 *   reading claims that ritual preserves identity and boundary norms
 *   primarily through symbolic experience and that survival is the continuity
 *   of practice itself — not through embedded practical knowledge
 *   (competence_transmission_reading) or a dual register of symbolic and
 *   practical (hybrid_encoding_reading). At t0 (immediate post-catastrophe),
 *   ritual practice is coordinative and genuinely functional — identity
 *   continuity through symbolic performance is existentially necessary when
 *   material and social structures have collapsed. By t80 (contemporary
 *   period), extractiveness has risen as secular alternatives emerge and the
 *   founding problem (identity loss through diaspora/trauma) has been
 *   substantially addressed by younger generations born into safety. Theater
 *   ratio rises sharply as enforcement effort increasingly defends ritual
 *   form itself rather than the original identity-preservation function. The
 *   constraint is claimed as tangled_rope: genuine coordination function
 *   persists (communal boundary maintenance, intergenerational recognition)
 *   but asymmetric extraction has accumulated as rabbinic authority maintains
 *   institutional control through ritual gatekeeping while secularized Jews
 *   bear the cost of mandatory symbolic conformity.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: agenda-setter, institutional power, analytical exit — sets and enforces ritual standards; collects legitimacy from claiming true continuity; benefits from institutional control
 *   - secularized_jews: payer, moderate power, identity-locked exit — bear cost of ritual conformity without original functional necessity; cannot exit without losing identity recognition
 *   - practicing_orthodox_communities: beneficiary, organized power — receive coordination benefit of standardized practice that maintains communal boundary and intergenerational continuity
 *   - post_catastrophe_survivors: beneficiary-and-payer, powerless, trapped — ritual continuation was survival at t0; becomes extractive burden as threat recedes
 *   - secular_jewish_intellectuals: excluded from legitimacy, moderate power, mobile — argue identity persists through secular transmission; excluded by rabbinic gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_survival__symbol_survival_reading, 0.71).
domain_priors:theater_ratio(catastrophe_memory_survival__symbol_survival_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(catastrophe_memory_survival__symbol_survival_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__symbol_survival_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__symbol_survival_reading, "Ritual Form Preservation and Identity Boundary Maintenance (Symbol-Survival Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__symbol_survival_reading, "religious/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__symbol_survival_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__symbol_survival_reading, '9f32e063-6634-43dd-9c6e-584ee49b4e0e').
narrative_ontology:cs_kernel_codification('9f32e063-6634-43dd-9c6e-584ee49b4e0e', fixed_text).
narrative_ontology:cs_authority_grounding('9f32e063-6634-43dd-9c6e-584ee49b4e0e', lineage).
narrative_ontology:cs_interpretation_layer_present('9f32e063-6634-43dd-9c6e-584ee49b4e0e').
narrative_ontology:cs_reading_relation('9f32e063-6634-43dd-9c6e-584ee49b4e0e', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f32e063-6634-43dd-9c6e-584ee49b4e0e', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('9f32e063-6634-43dd-9c6e-584ee49b4e0e', foundational, identity_through_symbolic_enactment).
narrative_ontology:cs_axiom_status(identity_through_symbolic_enactment, holdable).
narrative_ontology:cs_axiom_grounding('9f32e063-6634-43dd-9c6e-584ee49b4e0e', identity_through_symbolic_enactment, deontological).
narrative_ontology:cs_axiom('9f32e063-6634-43dd-9c6e-584ee49b4e0e', foundational, survival_as_practice_continuity).
narrative_ontology:cs_axiom_status(survival_as_practice_continuity, holdable).
narrative_ontology:cs_axiom_grounding('9f32e063-6634-43dd-9c6e-584ee49b4e0e', survival_as_practice_continuity, conventional).
narrative_ontology:cs_reference_frame('9f32e063-6634-43dd-9c6e-584ee49b4e0e', unbroken_ritual_transmission).
narrative_ontology:cs_drift_state('9f32e063-6634-43dd-9c6e-584ee49b4e0e', contemporary_secular_alternative_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f32e063-6634-43dd-9c6e-584ee49b4e0e', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, secularized_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, practicing_orthodox_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__symbol_survival_reading, post_catastrophe_survivors).
narrative_ontology:constraint_victim(catastrophe_memory_survival__symbol_survival_reading, post_catastrophe_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits the authoritative interpretation of ritual forms. Enforces ritual standardization and symbolic correctness through textual authority, communal sanction, and interpretive gatekeeping. Collects legitimacy and institutional power from claiming to hold the true continuity with pre-catastrophe tradition. The constraint's persistence strengthens their institutional position and epistemological authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority, beneficiary).

% Bear the cost of ritual participation required to maintain Jewish identity post-catastrophe. Experience the constraint as a demand for symbolic conformity and practice continuation when practical knowledge has attenuated or been rendered obsolete by historical rupture. Cannot exit without losing identity recognition; cannot stay without performing symbolic forms whose original functional content has degraded.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secularized_jews, payer,
    moderate, biographical, identity_locked, global).

% Receive the coordination benefit of standardized ritual practice that maintains communal boundaries and intergenerational continuity. Ritual forms create verifiable membership and shared behavioral scripts that hold the community together across dispersal and diaspora. The constraint's enforcement ensures that their children recognize and inherit their identity through practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, practicing_orthodox_communities, beneficiary,
    organized, generational, constrained, regional).

% In the immediate post-catastrophe period, ritual continuity offered a way to maintain identity when everything material was destroyed. Ritual performance was survival because identity-continuity was survival. Over time, as secular alternatives emerge and younger generations were born into safety, the constraint becomes extractive: the demand for ritual form persists even as its original function (maintaining identity under existential threat) has been accomplished.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, post_catastrophe_survivors, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__symbol_survival_reading, post_catastrophe_survivors, payer).

% Would argue that Jewish identity can persist and evolve through secular cultural transmission, historical memory, and ethical practice without mandatory ritual symbolic performance. They are structurally excluded from setting the terms of 'authentic' Jewish survival because rabbinic authority controls institutional legitimacy and community recognition. Their alternative reading of how identity persists is treated as assimilation or inauthenticity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, secular_jewish_intellectuals, excluded,
    moderate, biographical, mobile, global).

% Examines whether ritual form preservation functions primarily as symbolic boundary maintenance (this reading) or as embedded practical knowledge transmission (sibling reading) or both. Observes whether ritual continues because it serves survival functions or because institutional authority has ossified it into an end in itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__symbol_survival_reading, historian_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__symbol_survival_reading, rabbinic_authority).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__symbol_survival_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual standardization coordinates a dispersed Jewish population around shared symbolic practices and boundary markers, enabling identity recognition and communal membership across geographic separation and generational rupture following catastrophe.
% TRANSFER_FUNCTION: Moves the burden of continuous symbolic performance from rabbinic authority (who interprets and legitimates) to participants (who must enact and transmit), sustaining institutional religious authority in exchange for identity continuity and community membership.
% ABSENT_VOICES: Secular Jewish intellectuals and diaspora communities developing non-ritual identity transmission methods would argue that identity can persist through cultural memory and ethical practice; they are structurally excluded from legitimacy claims by rabbinic gatekeeping of 'authentic' Jewish survival. Post-catastrophe generations who did not experience the original function would argue the constraint has outlived its purpose.
% DISAPPEARANCE_RATIONALE: Orthodox stakeholders argue that ritual form disappearance means Jewish identity disappears — continuity of practice IS survival. Secular stakeholders argue Jewish identity and memory would reorganize around non-ritual modes (literature, history, ethical tradition) and community would strengthen, not vanish. The contest is whether the constraint's form is identical with its function or has become performative maintenance of institutional authority.
% FOUNDING_PROBLEM: Post-catastrophe: How does a dispersed, traumatized people maintain identity when material continuity is severed and generational transmission interrupted? Ritual practice offered a recoverable, portable, reproducible mechanism for identity persistence when nothing else remained stable.
% FOUNDING_PROBLEM_CORROBORATION: Immediate survivors attest the founding problem was existentially live. Contemporary historians and demographers document identity persistence through ritual when secular frameworks had collapsed. Secular Jewish communities attest the founding problem is substantially solved — identity persists through secular cultural institutions without mandatory ritual. Rabbinic authorities continue to claim the problem is live (identity is always under threat) and that ritual form is the only reliable solution; this claim is disputed by those who observe continuing Jewish identity among minimally-practicing Jews.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__symbol_survival_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__symbol_survival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__symbol_survival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__symbol_survival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__symbol_survival_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__symbol_survival_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__symbol_survival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.68 across 80 time units because the founding problem (identity loss through diaspora/catastrophe) has been substantially solved by time t20-40 (younger generations born into safety, secular institutions mature), but the constraint persists and intensifies. This is the signature pattern of a constraint whose original function has degraded but whose enforcement infrastructure remains entrenched. The theater ratio (performative-to-functional activity) rises sharply from 0.08 to 0.42, indicating that enforcement effort increasingly defends ritual form as an end-in-itself rather than as a means to the founding identity-preservation problem. Suppression rises from 0.45 to 0.71 because: (1) the constraint must work harder to persist when its original urgency has faded, and (2) secularized Jews increasingly encounter the constraint as a demand for conformity rather than as a necessary survival strategy. The asymmetry between beneficiary (rabbinic authority, who maintains control) and victims (secularized Jews, who bear conformity costs without receiving equivalent coordination benefit in a post-threat environment) is the tangled_rope signature: genuine coordination persists but extraction has detached from the original coordinated function.
 *
 * PERSPECTIVAL GAP:
 *   From rabbinic authority's seat: the constraint is pure coordination — ritual form standardization enables identity continuity and communal coherence; they are maintaining a timeless tradition against corrosive secular alternatives. From secularized Jews' seat: the constraint is increasingly extractive — it demands conformity to symbolic forms whose original survival function has been accomplished, enforced by institutional gatekeeping that excludes alternative identity-transmission modes. From practicing orthodox communities' seat: the constraint is coordinative; they perceive ritual performance as intrinsically valuable and identity-constitutive, not as a burden. The engine computes these seat-divergent classifications from the structural data (power, exit, beneficiary/victim position); the authored claim (tangled_rope) sits between the agenda-setter's reading (pure coordination) and the target's reading (extraction with a coordination cover story).
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority sits at d ≈ 0.15 (beneficiary end): they set the rules, interpret authoritative forms, collect institutional legitimacy and power from gatekeeping, and have analytical exit (they could change the rules but have no structural incentive to do so). Secularized Jews sit at d ≈ 0.82 (target end): they are identity-locked (cannot exit without losing recognition), moderate power (cannot unilaterally change the rules), and face rising cost of conformity as the original survival function has faded but enforcement persists. Practicing orthodox communities sit near d ≈ 0.35 (symmetric-to-slight-beneficiary): they receive genuine coordination benefits (boundary maintenance, intergenerational continuity, verifiable membership) and willingly participate; they bear costs but choose them as part of their worldview. Post-catastrophe survivors sit at d ≈ 0.70 (initially high target, fading to moderate as time passes): in the immediate period they perceive ritual as survival (low d initially), but the asymmetry between their sacrifice and younger generations' inheritance creates extraction as the original urgency fades.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: the founding mandate (preserve identity continuity post-catastrophe through ritual practice) has been substantially accomplished — younger generations born into stable Jewish communities in diaspora maintain identity without existential ritual dependence. Yet the constraint persists and intensifies. The theater_ratio rise from 0.08 to 0.42 indicates that enforcement effort increasingly defends ritual form as a symbolic boundary-marker and institutional authority anchor rather than as the identity-preservation mechanism it originally was. The measurement series captures the tipping point around t20-40 where the problem the constraint was built to solve has faded but the constraint's enforcement machinery remains intact and has reoriented toward institutional self-maintenance (gatekeeping, legitimacy collection, boundary defense). This is the piton-candidate signature: genuine coordination function persists but is now entangled with institutional inertia and extracted institutional benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_loss_through_desymbolization,
    'What is the structural necessity of symbolic ritual form for identity persistence? Can Jewish identity continue without mandatory ritual performance, or does semantic content (historical memory, ethical tradition, cultural practice) require symbolic enactment to sustain identity recognition?',
    'Empirical observation of Jewish identity persistence across ritual-minimal and ritual-intensive communities over generational spans; sociological measurement of identity stability in secular vs. religious populations; linguistic/anthropological analysis of whether symbolic performance is constitutive of identity or decorative.',
    'If identity persists robustly in secular-transmission modes, extractiveness reclassifies downward and the constraint becomes snare (institutional authority maintains ritual requirement for institutional benefit, not for identity preservation). If identity systematically degrades in ritual-minimal contexts, extractiveness classification holds and tangled_rope fits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_loss_through_desymbolization, conceptual, 'Whether symbolic ritual form is structurally necessary for identity persistence or whether identity can persist through non-ritual semantic transmission.').

omega_variable(
    founding_problem_evolution_vs_institutional_ossification,
    'Has the founding problem (identity loss through diaspora and catastrophic rupture) been substantially solved by time t80, or does rabbinic authority correctly claim that identity is always under existential threat and ritual form is the irreducible defense?',
    'Comparison of identity-persistence metrics across generations (t0 survivors vs. t80 descendants); measurement of threat perception among secular vs. religious Jewish communities; analysis of whether ritual-minimal Jews actually experience identity attenuation or whether they maintain identity through alternative mechanisms.',
    'If founding problem is solved: supports mandatrophy classification and piton reclassification; indicates the constraint persists through institutional inertia rather than functional necessity. If founding problem remains live: supports institutional reading of tangled_rope; indicates rabbinic authority is correct that ritual form remains identity-critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_evolution_vs_institutional_ossification, empirical, 'Whether the original post-catastrophe survival problem has been solved or remains perpetually live.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.71) primarily structural (legal/social barriers to non-ritual identity, institutional gatekeeping of legitimacy) or internalized (secularized Jews have absorbed the belief that identity is impossible without ritual, carry the suppression psychologically after exit)?',
    'Post-exit trajectory: measure identity confidence and Jewish self-identification among Jews who have openly rejected ritual in communities where secular Judaism is institutionally recognized vs. communities where ritual is mandatory. If suppression persists after structural barriers are removed, substantial internalization is indicated.',
    'If primarily structural: fixing the constraint requires removing institutional gatekeeping and recognizing secular Jewish identity; the constraint becomes a snare by reclassification. If internalized: exits from ritual still carry identity loss even where secular alternatives are institutionally available; the constraint''s effective extraction is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of alternative identity-transmission modes is structural (institutional gatekeeping) or internalized (psychological identity-fusion with ritual practice).').

omega_variable(
    reading_coexistence_or_foreclosure,
    'Do the symbol_survival and competence_transmission readings describe genuinely different constraints (decomposable via ε-invariance) or are they alternative descriptions of a single constraint whose dual function has been obscured?',
    'Ethnographic analysis of ritual practice: do practitioners perceive themselves as enacting symbolic identity-markers, or as transmitting practical knowledge about family rhythm, resource management, community protocols? Can a single ritual simultaneously satisfy both readings, or do they require different ritual emphases and different participant dispositions?',
    'If truly decomposable (ε-invariant difference): maintain as separate constraints with network.affects_constraints links. If they describe the same constraint: reclassify to hybrid_encoding_reading and treat symbol_survival and competence_transmission as partial framings of one dual-register constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_or_foreclosure, conceptual, 'Whether the symbol-survival and competence-transmission readings instantiate genuinely different constraints (structurally and epistemically distinct) or partial descriptions of a hybrid constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__symbol_survival_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__symbol_survival_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__symbol_survival_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__symbol_survival_reading, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__symbol_survival_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__symbol_survival_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__symbol_survival_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'catastrophe_memory_survival.' The symbol_survival_reading instantiates high extractiveness (ε=0.68) centered on ritual form preservation as identity mechanism, with rabbinic authority as beneficiary and secularized Jews as victims. Sibling readings decompose the kernel by emphasizing different functional mechanisms (practical knowledge transmission vs. dual symbolic+practical registers). All three readings share the same kernel commitment (Jewish identity persists after catastrophe through ritual practice) but diverge on what ritual carries and preserves. The three stories together map the contested space of post-catastrophe Jewish survival.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__symbol_survival_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
