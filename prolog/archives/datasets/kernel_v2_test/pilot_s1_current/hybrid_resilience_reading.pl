% ============================================================================
% CONSTRAINT STORY: hybrid_resilience_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_resilience_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hybrid_resilience_reading
 *   human_readable: Dual-Function Ritual Encoding: Identity-Continuity and Adaptive Capacity Co-Evolution
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual structures in traditional societies often encode adaptive
 *   knowledge (seasonal indicators, hazard response protocols, medicinal
 *   techniques, social coordination mechanisms) within symbolic forms (myth,
 *   ceremony, taboo, sacred narrative). This constraint maps the structural
 *   tension between maintaining symbolic continuity (which preserves cultural
 *   identity and legitimacy across catastrophe and rupture) and preserving
 *   adaptive capacity (which ensures practical survival competence remains
 *   accessible and evolvable). The hybrid_resilience_reading asserts that
 *   successful long-duration cultures co-evolve both dimensions through a
 *   dual-encoding strategy: ritual forms carry both symbolic meaning-content
 *   AND embedded practical knowledge, and specialist interpreters (elders,
 *   priests, healers) actively manage the relationship between them.
 *   Extraction arises when institutions privilege one dimension (symbol or
 *   function) at the expense of the other, or when the encoding relationship
 *   becomes opaque to the community. Theater increases when ritual form
 *   becomes formally preserved but functionally divorced from its original
 *   adaptive purpose — the institution enforces the form through prescription
 *   and correction, but the knowledge-transmission function atrophies into
 *   ceremonial performance. The constraint's victims are communities that
 *   collapse either dimension: those bound to literal form-replication
 *   without understanding practical content, and those maintaining technique
 *   without the symbolic meaning-system that originally contextualized it.
 *
 * KEY AGENTS:
 *   - Continuity-Maintaining Populations: Primary beneficiary (organized/mobile) — populations that successfully integrate both symbolic fidelity and functional innovation through active interpretive management. Benefit from ritual's dual-encoding through cultural coherence and practical knowledge transmission.
 *   - Symbolically Rigid Communities: Primary victim (powerless/trapped) — communities bound to literal form-replication without access to the adaptive knowledge the ritual encodes. Trapped by identity fusion with inherited form; cannot break from ritual without becoming 'not us.' Suppression enforces form-replication even when forms become dysfunctional in changed conditions.
 *   - Operationally Isolated Communities: Primary victim (powerless/identity_locked) — communities that maintain ritual technique but have severed symbolic continuity links. Know how to perform but not why; cannot reintegrate meaning without threatening mastery-identity. Extraction via cognitive separation of form and function.
 *   - Interpretive Authority (Religious Specialists): Secondary beneficiary (organized/mobile) — elders, priests, liturgists who actively encode and decode dual-function structure. Status and authority grounded in management of the symbolic-functional tension. See constraint as coordination mechanism for knowledge transmission.
 *   - Religious Institutions (Formal): Secondary actor (institutional/arbitrage) — churches, denominations, liturgical bodies that codify ritual form through canon law and prescription. Interest in form-preservation and institutional continuity. Theater increases through formalization and gatekeeping.
 *   - Lineage Renewal Movements: Tertiary actor (organized/constrained) — communities that deliberately reconstruct the symbolic-functional link through genealogical recovery. See constraint as temporary, with sunset logic as reintegration progresses.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees dual-function encoding as structural solution to cultural transmission, risking naturalization of what may be contingent institutional arrangements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_resilience_reading, 0.32).
domain_priors:suppression_score(hybrid_resilience_reading, 0.48).
domain_priors:theater_ratio(hybrid_resilience_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_resilience_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(hybrid_resilience_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(hybrid_resilience_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_resilience_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_resilience_reading, "Dual-Function Ritual Encoding: Identity-Continuity and Adaptive Capacity Co-Evolution").
narrative_ontology:topic_domain(hybrid_resilience_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(hybrid_resilience_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_resilience_reading, 'c611d1fd-5911-4364-9f11-4f5a799fa4c4').
narrative_ontology:cs_kernel_codification('c611d1fd-5911-4364-9f11-4f5a799fa4c4', distributed).
narrative_ontology:cs_authority_grounding('c611d1fd-5911-4364-9f11-4f5a799fa4c4', lineage).
narrative_ontology:cs_interpretation_layer_present('c611d1fd-5911-4364-9f11-4f5a799fa4c4').
narrative_ontology:cs_reading_relation('c611d1fd-5911-4364-9f11-4f5a799fa4c4', hybrid_resilience_reading__symbolic_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c611d1fd-5911-4364-9f11-4f5a799fa4c4', hybrid_resilience_reading__adaptive_competence_reading, coexists_with).
narrative_ontology:cs_axiom('c611d1fd-5911-4364-9f11-4f5a799fa4c4', foundational, form_function_co_evolution_necessity).
narrative_ontology:cs_axiom_status(form_function_co_evolution_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c611d1fd-5911-4364-9f11-4f5a799fa4c4', form_function_co_evolution_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c611d1fd-5911-4364-9f11-4f5a799fa4c4', foundational, institutional_privilege_extraction_mechanism).
narrative_ontology:cs_axiom_status(institutional_privilege_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c611d1fd-5911-4364-9f11-4f5a799fa4c4', institutional_privilege_extraction_mechanism, deontological).
narrative_ontology:cs_reference_frame('c611d1fd-5911-4364-9f11-4f5a799fa4c4', dual_encoded_transmission_intact).
narrative_ontology:cs_drift_state('c611d1fd-5911-4364-9f11-4f5a799fa4c4', contemporary_institutional_gatekeeping_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c611d1fd-5911-4364-9f11-4f5a799fa4c4', '').
narrative_ontology:cs_kernel_id(hybrid_resilience_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_resilience_reading, continuity_maintaining_populations).
narrative_ontology:constraint_victim(hybrid_resilience_reading, symbolically_rigid_communities).
narrative_ontology:constraint_victim(hybrid_resilience_reading, operationally_isolated_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Communities bound to literal form-replication without understanding embedded adaptive content. Faces maximum extraction: must maintain rituals exactly as transmitted (suppresses innovation and local adaptation) while losing practical competence the ritual originally encoded. Trapped by identity fusion with inherited form — cannot break from the ritual without becoming 'not us,' yet the ritual provides no functional survival benefit in changed conditions.
constraint_indexing:constraint_classification(hybrid_resilience_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Communities that maintain ritual competence but have severed the symbolic continuity link to founding myths and legitimacy narratives. Experiences extraction through a different mechanism: ritual knowledge is maintained as bare technique, drained of meaning-bearing content, orphaned from the cultural identity that gives it purpose. Exit would require abandoning hard-won practical knowledge and the identity as 'the keepers of the technique.' Trapped by cognitive capture in the form/function split — cannot reintegrate the symbolic dimension without threatening the operational mastery they have preserved.
constraint_indexing:constraint_classification(hybrid_resilience_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% Communities actively managing the tension between fidelity to inherited form and responsiveness to changed conditions. Coordinates both dimensions: maintains symbolic continuity through reinterpretation, preserves operative competence through selective innovation. Experiences both genuine coordination benefit (ritual provides cultural coherence and practical transmission) and extraction cost (must navigate community tension between innovators and traditionalists; constrained by social penalty for appearing to break sacred form). Moderate power — enough agency to attempt dual management but insufficient to eliminate the constraint's friction.
constraint_indexing:constraint_classification(hybrid_resilience_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Religious specialists, elders, liturgical scholars who actively encode and decode the dual-function structure. See the constraint as coordination: their role is precisely to transmit both symbolic form AND operational meaning to the next generation, mediating between literal replication and functional innovation. Net beneficiary through status and intellectual authority. Mobile — they could exit the interpretive role but choose not to; the constraint serves their interests in maintaining cultural authority and knowledge transmission.
constraint_indexing:constraint_classification(hybrid_resilience_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% Institutional gatekeepers (churches, denominations, formal liturgical bodies) that maintain ritual codification through canon law, written liturgy, and doctrinal prescription. The institution's interest is in form-preservation and institutional continuity rather than functional adaptation. Theater ratio indicates that much institutional ritual maintenance is performative formality divorced from adaptive function — the institution maintains the form through regularization and correction against 'deviation,' but the original function-encoding is partially degraded into ceremonial theater. Piton perspective: the institution enforces ritual form through inertial authority rather than live function.
constraint_indexing:constraint_classification(hybrid_resilience_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Communities that explicitly reconstruct the dual-function link through genealogical recovery: tracing ritual forms back to their original ecological or social context to recover the adaptive knowledge they encoded. Sees the constraint as temporary — the phase of severed symbolic-operational linkage as something that can be overcome through deliberate reconstruction. Has sunset logic: as ecological knowledge is reintegrated into ritual meaning-making, the constraint dissolves. Constrained by knowledge-loss (some adaptive content may be permanently lost) but organized enough to actively pursue recovery.
constraint_indexing:constraint_classification(hybrid_resilience_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% From a universal/civilizational perspective, the co-evolution of symbolic continuity and adaptive capacity is a structural feature of cultural transmission itself — any system that preserves knowledge across rupture must encode both fidelity (so what matters persists) and flexibility (so what matters can be applied to new conditions). This perspective sees ritual as an immutable solution to an immutable problem: the cognitive and social requirements of long-term cultural memory. However, the structured data reveals false-summit risk: the constraint's beneficiaries and enforcement requirements suggest the 'natural law' framing may naturalize specific institutional choices about which dimension (symbolic vs operational) gets privileged, suppressing awareness of the contingent extraction mechanisms.
constraint_indexing:constraint_classification(hybrid_resilience_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_resilience_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_resilience_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_resilience_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_resilience_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_resilience_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The constraint does coordinate genuine knowledge transmission — symbolic form preserves cultural identity and provides mnemonic anchoring that enables multi-generational retention. But extraction occurs where institutions privilege form over function, or where communities lose access to the functional content entirely. The moderate level reflects that the coordination benefit is real but asymmetric: beneficiaries (interpretive authorities and adapting communities) benefit substantially, while victims (form-rigid and function-isolated communities) bear suppression costs without functional gain. Theater ratio (0.41): Moderate. In adaptive communities actively managing both dimensions, ritual is relatively functional — theater is low, form carries real meaning and practical content. As communities drift toward form-rigidity or institutional gatekeeping, theater rises (t0=0.28 to t5=0.41). The slight decline at t8 reflects some communities actively recovering functional content (lineage renewal effect). Suppression (0.48): Moderate-high. Enforcing symbolic form-fidelity while suppressing functional innovation requires sustained institutional pressure and epistemic control (restricting who may interpret, limiting knowledge access, stigmatizing deviation). However, suppression is not total — oral tradition maintains alternative transmission pathways, and communities can and do break free through deliberate reconstruction. The declining trajectory (t0=0.52 to t8=0.45) reflects growing counter-movements toward genealogical recovery and interpretive democratization.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the reading's core structural claim: the constraint is not simply form-preservation or functional transmission, but the RELATIONSHIP BETWEEN them. Form-rigid communities see the constraint as mountain-like (immutable identity requirement) or snare-like (trapped by sacred obligation). Function-isolated communities see it as degraded (piton) or orphaned (snare without meaning). Adapting communities see tangled rope (genuine coordination with real extraction friction). Interpretive authorities see rope (pure coordination). Institutional gatekeepers see piton (performative formality). Lineage renewal sees scaffold (temporary, solvable through recovery). The analytical observer risks seeing mountain (natural law of cultural transmission) but structural data reveals false-summit risk: the 'naturalness' may rationalize institutional choices to privilege form over function. The gap between powerless and organized perspectives is maximal: powerless agents experience extraction and suppression; organized agents experience beneficiary status and agency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint maps to the agent's structural relationship to the form-function tension. Interpretive authorities and adapting communities have low d (beneficiaries with agency) — they can navigate both dimensions and benefit from the dual-encoding. Symbolically rigid communities have high d (trapped victims) — they must replicate form without understanding content, bearing suppression costs. Operationally isolated communities have moderate-high d (identity-locked targets) — they maintain competence but are cognitively captured in the form-function split, unable to reintegrate the symbolic dimension. The engine derives d from beneficiary/victim declarations plus exit modulation: trapped exit maximizes d; mobile exit minimizes d; constrained and identity_locked intermediate. Effective extraction (χ) is scaled by scope (local rituals show less χ amplification than continental religious traditions) and by the agent's directionality. Powerless agents with trapped exit experience maximum χ; institutional agents with arbitrage exit experience minimal or negative χ.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through kernel reading. The mandate — preserving cultural identity and adaptive capacity across catastrophe — remains live and functional. But the specific institutional mechanisms for carrying the mandate (formal liturgy, prescription-based canon, gatekeeper authority) have partially atrophied: they preserve form at the expense of function, and the dual-encoding relationship has become opaque. The constraint is mandatrophic in Perspective 5 (formal institution) but not in Perspectives 3-4 (adapting communities, interpretive specialists). The resolution is not that the mandate has outlived its function, but that the form OF mandate-carrying has become partially dysfunctional. Lineage renewal movements represent deliberate mandate-recovery: restoring the functional content to the symbolic form, reintegrating the encoding relationship. The constraint remains UNRESOLVED mandatrophy in the sense that institutional gatekeeping still privileges form over function, but SOLVABLE mandatrophy in that the solution (genealogical recovery, reinterpretation of inherited forms to extract their adaptive content) is demonstrable and actively pursued.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_decoupling_origin,
    'Is the split between symbolic form and adaptive function an inherent feature of long-duration ritual transmission, or a contingent institutional pathology introduced by specific configurations (scribal liturgies, written canon, institutional gatekeeping)?',
    'Comparative historical analysis: oral-tradition societies where form-function integration persists vs literate-institutional societies where decoupling occurs; cases of deliberate reintegration (lineage renewal) and measurement of success; ethnographic documentation of societies maintaining both dimensions simultaneously.',
    'If inherent: the mountain classification is correct and the constraint cannot be escaped, only managed. Piton perspective becomes necessary. If contingent: the constraint is an extractive institutional innovation masquerading as natural law — alternative transmission structures (oral mentorship, embodied learning, genealogical reconstruction) represent genuine exits. Classification shifts from mountain toward snare+scaffold cluster.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_decoupling_origin, empirical, 'Whether form-function decoupling is inherent or contingent to ritual transmission').

omega_variable(
    dual_encoding_recognizability,
    'Can practitioners reliably identify which parts of an inherited ritual encode adaptive knowledge and which encode pure symbolic fidelity? Is the ''reading'' of dual function transparent to the community, or exclusively held by specialist interpreters?',
    'Ethnographic interviews: ask practitioners to explain which ritual elements have practical (hunting, agricultural, medical, social coordination) functions and which are pure symbolic transmission; compare explanations across knowledge levels (elders vs youth, specialists vs community); test whether decodification is possible by outsider with training.',
    'If transparent and widely recognized: the constraint is genuinely a coordination mechanism (Rope). Communities manage both dimensions with full awareness. If opaque/specialist-held: the constraint involves substantial suppression — communities follow forms without understanding them, creating vulnerability to institutional manipulation. Piton and Snare classifications become stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_encoding_recognizability, empirical, 'Whether dual-encoding is recognizable or specialist-held knowledge').

omega_variable(
    innovation_suppression_cost,
    'How much actual adaptive capacity is lost when communities prioritize symbolic form-fidelity over functional innovation? Can rituals be modified in ways that preserve adaptive intent while changing form?',
    'Comparative study: communities that enforce form-rigidity vs those that permit form-modification while maintaining symbolic meaning; measure outcomes (survival, knowledge transmission, cultural continuity) across generations; identify which ritual functions (seasonal coordination, hazard response, knowledge testing, social cohesion) are preserved vs lost under each regime.',
    'High innovation loss: the constraint''s extraction component is severe, justified only by collective-memory preservation. Suppression (0.48) may be understated. If adaptive capacity can be maintained through form-innovation: the constraint is primarily symbolic coordination (Rope/Scaffold rather than Snare/Tangled Rope). Beneficiary structure may shift from continuity-preservers to form-enforcers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_suppression_cost, empirical, 'Costs of form-fidelity enforcement vs permitting functional innovation').

omega_variable(
    reading_alternative_framing,
    'This constraint is ONE reading of the catastrophe_memory_transmission kernel. The alternative readings (symbolic_continuity_reading emphasizes form-preservation as primary, adaptive_competence_reading emphasizes functional transmission as primary) would assign different victim sets and different suppression targets. Does this reading''s dual-function framing represent a genuine structural discovery, or does it rationalize a tension that genuinely forecloses the alternatives?',
    'Examine cases where communities made explicit choices: prioritize symbolic fidelity OR prioritize adaptive competence. Do such communities succeed or fail? Do successful communities later shift back toward dual-function? Can a single community hold both readings simultaneously, or does choosing one foreclose the other? Historical analysis of schisms, reforms, and lineage rivalries.',
    'If dual-function is a genuine structural co-possibility: reading_relations should mark the alternatives as coexists_with. If choosing one reading (e.g., ''symbol is primary'') materially forecloses the other (''function is co-primary in equal measure''): reading_relations should use forecloses. If one reading is empirically superior but not logically impossible: reading_relations should use influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_alternative_framing, conceptual, 'Whether dual-function reading forecloses or coexists with alternative memory-transmission readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_resilience_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hyb_res_theater_t0, hybrid_resilience_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hyb_res_theater_t2, hybrid_resilience_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(hyb_res_theater_t5, hybrid_resilience_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(hyb_res_theater_t8, hybrid_resilience_reading, theater_ratio, 8, 0.39).

% Extraction over time
narrative_ontology:measurement(hyb_res_extract_t0, hybrid_resilience_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(hyb_res_extract_t2, hybrid_resilience_reading, base_extractiveness, 2, 0.28).
narrative_ontology:measurement(hyb_res_extract_t5, hybrid_resilience_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(hyb_res_extract_t8, hybrid_resilience_reading, base_extractiveness, 8, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(hyb_res_suppress_t0, hybrid_resilience_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(hyb_res_suppress_t3, hybrid_resilience_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(hyb_res_suppress_t5, hybrid_resilience_reading, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(hyb_res_suppress_t8, hybrid_resilience_reading, suppression_requirement, 8, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_resilience_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hybrid_resilience_reading, 0.12).
narrative_ontology:affects_constraint(hybrid_resilience_reading, symbolic_continuity_reading).
narrative_ontology:affects_constraint(hybrid_resilience_reading, adaptive_competence_reading).
narrative_ontology:affects_constraint(hybrid_resilience_reading, ecological_knowledge_codification).
narrative_ontology:affects_constraint(hybrid_resilience_reading, ritual_suppression_of_innovation).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_transmission kernel is instantiated by three structurally distinct constraints: symbolic_continuity_reading (emphasizes form-preservation, high theater when divorced from function), adaptive_competence_reading (emphasizes functional transmission, vulnerable to meaning-loss), and this hybrid_resilience_reading (emphasizes dual-encoding relationship, vulnerable when institutions privilege one dimension). Each has its own ε value reflecting different extraction mechanisms. The three readings coexist as live positions held by different authority structures and communities. This story links to ecological_knowledge_codification (which examines the specific practices by which practical knowledge gets embedded in ritual form) and ritual_suppression_of_innovation (which examines the enforcement mechanisms that maintain form-rigidity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hybrid_resilience_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
