% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Ritual-Fidelity Transmission of Survival Competence (Hybrid Embedded Reading)
 *   domain: religious/cultural
 *
 * SUMMARY:
 *   A community that survived a catastrophic flood binds its survival
 *   competence — storage discipline, evacuation movement, mutual-aid
 *   rotation, threat recognition — to recurring observances, on the
 *   hybrid-embedded thesis that the competence lives in the enacted form and
 *   cannot be lifted out of it. Fidelity to form is therefore not decoration
 *   around a transferable core; it is the transmission channel itself. The
 *   claim/metric relationship is deliberate and independent: the reading
 *   CLAIMS rope (coordination through shared practice, participants net
 *   beneficiaries, no victim class), while the metrics are authored from the
 *   arrangement's observable operation — low but rising extraction as
 *   catastrophe memory fades, low theater because the performance is the
 *   mechanism, moderate accessibility collapse because the propositional
 *   substitute demonstrably under-transfers. KEY AGENTS (by structural
 *   relationship): - practicing_community: primary beneficiary and
 *   simultaneous cost-bearer (organized/constrained) — rehearses, receives
 *   capacity - descendant_generations: silent beneficiary (powerless/trapped)
 *   — inherits whatever is preserved, cannot consent - ritual_officiants:
 *   administrator and collector (moderate/identity_locked) — runs fidelity,
 *   gains office and standing - reformist_members: cost-bearer pressing for
 *   form-change (moderate/constrained) — pays full observance costs plus
 *   reform friction - emergency_management_agencies: excluded outsider
 *   (institutional/arbitrage) — wants the content without the form, locked
 *   out - ritual_scholars: analytical observer (analytical/analytical) —
 *   measures transmission outcomes across communities Constraint-family note
 *   (epsilon decomposition): this file instantiates ONE reading of the kernel
 *   catastrophe_memory_transmission. The sibling readings author different
 *   epsilon values over the SAME standing arrangement — the
 *   operational_competence_reading reads the fidelity obligations as
 *   over-specification blocking an optimizable transmission channel (higher
 *   apparent extraction), and the symbol_continuity_reading reads the same
 *   obligations as intrinsically justified identity practice (low extraction,
 *   different justification). Same referent, different readings, different
 *   epsilon — hence separate files linked by network.affects_constraints
 *   rather than one story hedged across readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.24).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Ritual-Fidelity Transmission of Survival Competence (Hybrid Embedded Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, 'ebf6ef6a-1bd4-4911-aeca-87478f320263').
narrative_ontology:cs_kernel_codification('ebf6ef6a-1bd4-4911-aeca-87478f320263', distributed).
narrative_ontology:cs_authority_grounding('ebf6ef6a-1bd4-4911-aeca-87478f320263', practice).
narrative_ontology:cs_interpretation_layer_present('ebf6ef6a-1bd4-4911-aeca-87478f320263').
narrative_ontology:cs_reading_relation('ebf6ef6a-1bd4-4911-aeca-87478f320263', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebf6ef6a-1bd4-4911-aeca-87478f320263', catastrophe_memory_transmission__operational_competence_reading, influences).
narrative_ontology:cs_axiom('ebf6ef6a-1bd4-4911-aeca-87478f320263', foundational, form_function_inseparability).
narrative_ontology:cs_axiom_status(form_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('ebf6ef6a-1bd4-4911-aeca-87478f320263', form_function_inseparability, empirically_contingent).
narrative_ontology:cs_axiom('ebf6ef6a-1bd4-4911-aeca-87478f320263', secondary, fidelity_preserves_operational_capacity).
narrative_ontology:cs_axiom_status(fidelity_preserves_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('ebf6ef6a-1bd4-4911-aeca-87478f320263', fidelity_preserves_operational_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('ebf6ef6a-1bd4-4911-aeca-87478f320263', form_function_coconstitution).
narrative_ontology:cs_drift_state('ebf6ef6a-1bd4-4911-aeca-87478f320263', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ebf6ef6a-1bd4-4911-aeca-87478f320263', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, reformist_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts the seasonal and commemorative observances together: storage disciplines, movement drills, mutual-aid rotations, mourning rites. Each member gives hours of every year to rehearsal and receives, in return, a body that knows what to do when the water rises or the stores run low. Leaving the practice means losing both the trained reflexes and the community that would execute them alongside you.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, practicing_community, payer).

% Not yet present. They will inherit whatever capacity the current practice preserves or lets lapse, and they cannot attend, object to, or consent to any decision made now about which forms are shortened, merged, or dropped. Everything decided about fidelity today is decided on their behalf.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, descendant_generations, beneficiary,
    powerless, civilizational, trapped, regional).

% Trained from childhood, they lead the enactments, correct deviation in form, and adjudicate disputes about what counts as faithful observance. Office, daily purpose, and standing in the community flow from the practice's centrality; setting it aside would mean relinquishing role, authority, and self-understanding at once.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants, agenda_setter,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants, beneficiary).

% Propose abbreviating the calendar, translating the liturgy, or replacing rehearsal with documented procedure. Their proposals are repeatedly declined on the ground that altered form degrades what the form carries. They keep paying the full time cost of unmodified observance while absorbing the friction of arguing for change from inside; leaving would cost them family and community ties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, reformist_members, payer,
    moderate, biographical, constrained, regional).

% Secular bodies that can see the community's hazard-response performance and want the underlying competence replicated at scale. They cannot obtain it except by engaging the practice on its own terms, which their mandates do not accommodate, so they build parallel training curricula instead and treat the community's refusal to hand over content as obstinacy rather than structure.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, emergency_management_agencies, excluded,
    institutional, immediate, arbitrage, national).

% Ethnographers and cognitive scientists who compare transmission outcomes across communities that preserved, altered, or abandoned their observances. They hold no stake in fidelity or reform and publish what the comparisons show, including when the results flatter no party.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_officiants).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__hybrid_embedded_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes communal rehearsal so that hazard-relevant capacities — storage discipline, evacuation movement, mutual-aid rotation, threat recognition — are re-enacted frequently enough to remain embodied across generational turnover, without depending on written instruction that the founding generation found insufficient.
% TRANSFER_FUNCTION: Moves embodied operational capacity from older to younger practitioners through repeated co-enactment; moves hours of every member's year into rehearsal; moves office, standing, and interpretive authority to the officiants who administer fidelity.
% ABSENT_VOICES: Descendant generations, on whose behalf every fidelity decision is made, are structurally absent. Lapsed members who left the practice could testify to what persisted and what decayed after departure but are rarely consulted. Secular emergency planners stand outside the deliberation entirely; their interest in the content without the form never enters the room.
% DISAPPEARANCE_RATIONALE: If the observances ceased overnight, the rehearsal schedule that keeps storage discipline, movement drills, and mutual-aid rotation in the body would stop. Records would survive but reflexes would not: within two generations the community would hold documents describing what it could no longer do, and neighboring secular institutions would backfill with slower, less reliable programs built from scratch.
% FOUNDING_PROBLEM: After a catastrophic flood, survivors possessed knowledge that had saved lives but could not be conveyed by instruction alone to descendants who had not lived through the event. They bound the knowledge to recurring observances so that each generation would rehearse it bodily before needing it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: cognitive-science literature on procedural learning attests that description under-transmits what repetition encodes; disaster sociology finds better outcomes in communities that retained practice than in those that archived it; survivor oral histories record the encoding intent directly. Descendant generations themselves cannot yet attest — their absence is part of the finding, not a refutation of it.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__hybrid_embedded_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__hybrid_embedded_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__hybrid_embedded_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__hybrid_embedded_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).
:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22 at interval end) because the arrangement's costs — hours of rehearsal, rigidity of calendar — are the transmission mechanism itself under this reading, and the participating community is its own primary beneficiary. The slow rise across the interval tracks fading catastrophe memory: as the founding event recedes, the cost side of the ledger stays visible while the function side becomes an article of inherited belief, so the net burden creeps upward without any change in the observances themselves. Suppression (0.24) is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation; the fidelity pressure here is mostly reputational and internalized rather than backed by coercive machinery, hence requires_active_enforcement is false. Theater_ratio is authored LOW (0.16) and this is the analytically decisive choice: to an outside eye the observances look maximally performative, but under the co-constitution thesis the performance IS the function, so theatricality must not be inferred from outward form. The small upward drift models elements whose connection to function has thinned while fidelity norms keep them in place. Accessibility_collapse (0.55) sits mid-range: the propositional substitute (manuals, curricula) is known to under-transfer, collapsing that alternative substantially, but partial channels — apprenticeship outside the ritual frame, video documentation, drill programs — remain workable at reduced fidelity. Resistance (0.30) reflects steady reformist pressure rather than open conflict. The measurement series run on one shared time grid (points 0-60 at decade spacing) with every tracked metric authored at every point, so no end-state value is silently substituted into earlier rows.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the finding. From the officiants' seat the arrangement is the thing they are: administering fidelity is office, identity, and daily purpose, and the classification computed there should sit near the beneficiary pole with identity-lock amplifying stability. From the reformist seat the same observances are a rigid tax paid in time with proposals for relief always declined — nearer the target pole, with constrained exit keeping them exposed. Descendant generations compute as pure beneficiaries with zero agency: maximal subsidy, no voice. Emergency management agencies experience the arrangement as a closed shop — they see the output (competent hazard response) and are denied the input (the content), which from their seat looks like artificial scarcity even though this reading locates the scarcity in the knowledge's structure rather than in anyone's choice. The engine derives these divergent classifications from the structural data; the authored rope claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: practicing_community, descendant_generations, and ritual_officiants are declared beneficiaries, pulling their directionalities toward the subsidized end — descendant_generations furthest of all, since they are trapped recipients who bear none of the current costs. The community's dual position (beneficiary with a real payer secondary role) should land it near symmetric, which is exactly what a healthy coordination arrangement looks like from inside. Reformist_members are declared through their payer role with constrained exit, pushing them toward the target end — they bear the arrangement's costs while disputing its necessity. Emergency_management_agencies are excluded rather than coordinated: they bear no internal costs and are denied the gains, a position the beneficiary/victim derivation alone does not capture, which is why their exclusion is documented in the stakeholder situation and the absent_voices answer rather than forced into a directionality number. No directionality overrides are authored: the structural declarations produce the right relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk in this domain runs in the unusual direction: the failure mode is mislabeling living coordination as dead performance. Because ritual is outwardly theatrical, a naive theater-based reading would classify the observances as piton — vestigial ceremony maintained by inertia. This story guards against that by authoring theater_ratio from the co-constitution thesis (the performance is the mechanism) rather than from appearance, and by anchoring the founding-problem interview: the founding problem (bodily transmission of unteachable competence across generational turnover) is LIVE, since generational turnover guarantees recurrence regardless of whether the original flood recurs. The reverse guard is also in place: if the inseparability thesis fails empirically (omega inseparability_empirical_status) or the practice loses contact with real hazards without detecting it (omega obsolete_function_detection), the same fidelity norms that once carried function become pure overhead, and the arrangement's honest trajectory bends toward inertial persistence. The rising base_extractiveness series is the early signature of that bend; it is authored honestly rather than tuned flat to protect the rope claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the kernel of catastrophe-memory transmission correctly described by this hybrid reading (form and function co-constitutive), by the operational_competence_reading (ritual as a describable encoding of pattern recognition, resource coordination, and threat rehearsal), or by the symbol_continuity_reading (symbolic form preserved as intrinsic communal good)?',
    'Comparative ethnography of communities that altered versus preserved their observances, tracking both capacity retention and identity continuity over multiple generations, with pre-registered measures agreed by all three reading-parties.',
    'If the operational reading is right, forms are optimizable instruments and fidelity obligations beyond measured function are over-specification; if the symbol reading is right, function language is cover and fidelity is an identity duty answerable to no performance metric. Each outcome restructures this reading''s beneficiary arithmetic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading of the catastrophe-memory kernel correctly locates the constraint''s center of gravity.').

omega_variable(
    inseparability_empirical_status,
    'Is the embedded survival competence genuinely non-propositional and inseparable from enacted form, or could structured apprenticeship and documented curricula transmit it equally well?',
    'Controlled comparison of ritual-trained cohorts against curriculum-trained cohorts on standardized hazard-response tasks, with retention measured at five- and ten-year intervals.',
    'If separable, the fidelity requirement imposes costs beyond what transmission needs and the effective burden on members rises accordingly; if inseparable, fidelity is load-bearing and the measured costs are the price of the capacity itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inseparability_empirical_status, empirical, 'Whether the reading''s core inseparability claim survives controlled transfer testing.').

omega_variable(
    obsolete_function_detection,
    'When the environment shifts so that the rehearsed scenarios no longer track real hazards, can the community detect the mismatch — given that this very reading attributes hidden function to form and therefore predicts that apparently empty elements may still be carrying content?',
    'External audit correlating the practiced scenario inventory against current regional hazard data, conducted by analysts with no standing in the fidelity dispute.',
    'If obsolescence is undetectable from inside, fidelity costs convert gradually into pure overhead and the arrangement drifts toward inertial persistence that its own theory cannot self-correct; if detectable, the practice retains a feedback path and the drift reverses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(obsolete_function_detection, empirical, 'Whether the co-constitution thesis blinds the practice to its own obsolescence.').

omega_variable(
    suppression_internalization_split,
    'Is the fidelity pressure members experience structural (communal sanction on deviating households) or internalized (self-policing obligation that persists independent of sanction)?',
    'Post-departure interviews with lapsed members: if felt obligation and rehearsal compulsion persist after exit and after sanction exposure ends, the internalized share is substantial.',
    'If largely internalized, the effective suppression exceeds what the structural measure shows — members carry the fidelity demand with them after leaving, and reform pressure inside the community is weaker than the visible dissent suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized composition of the fidelity pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 60, 0.16).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 40, 0.19).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.21).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 60, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_transmission__hybrid_embedded_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual transmits survival knowledge' decomposes into three structurally distinct constraints sharing one kernel (catastrophe_memory_transmission), split per the epsilon-invariance principle: the hybrid_embedded_reading (this file — co-constitution, epsilon ~0.22, rope), the operational_competence_reading (describable encoding, optimizable vehicle — would author higher epsilon for the same fidelity obligations, read as over-specification), and the symbol_continuity_reading (form as intrinsic good — low epsilon justified by identity value rather than function). The upstream/downstream structure runs from this reading outward: the inseparability thesis sets the evidential bar that any extraction-style pedagogy must beat, and the symbol reading draws its practical force from the failures of extraction attempts that this reading predicts. All three files link one another through network.affects_constraints; no single story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
