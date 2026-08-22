% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__gradual_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__gradual_transition_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__gradual_transition_reading
 *   human_readable: Turkish Dual-Script Literacy Transition (Gradual Reading)
 *   domain: political/linguistic/cultural
 *
 * SUMMARY:
 *   After the collapse of the Ottoman Empire and the establishment of the
 *   Turkish nation-state, Turkish language was confronted with a graphemic
 *   choice: retain Arabic script (the Ottoman substrate, marking continuity
 *   with Islamic civilization) or adopt Latin script (marking alignment with
 *   European modernity and symbolic rupture from the Ottoman past). The
 *   Turkish state, under Kemal Atatürk, chose rapid Latinization in 1928. The
 *   gradual-transition reading posits an alternative: a 5-15 year window
 *   during which both scripts coexist in education, administration, and
 *   culture, preserving intergenerational knowledge transfer while enabling
 *   modernization. This reading is a KERNEL READING: it instantiates one
 *   interpretation of the contested 'turkish_graphemic_substrate' kernel,
 *   alongside the ottoman_continuity_reading (Arabic script is the legitimate
 *   substrate) and the secular_nationalist_reading (Latin script is the only
 *   legitimate modern substrate). The constraint described here is what the
 *   gradual-transition reading CREATES: a managed coexistence that extracts
 *   from younger generations (dual-literacy burden) and the state apparatus
 *   (dual infrastructure costs) while benefiting older educated populations
 *   (literacy credentials preserved) and cultural continuity constituencies
 *   (access to heritage texts). The extractiveness is moderate (0.62 at
 *   interval end) because the constraint solves a real coordination problem
 *   (knowledge transfer + modernization) but does so by imposing costs on
 *   specific seats rather than distributing them. Suppression is moderate
 *   (0.58) because resistance is substantial—secular nationalists push for
 *   acceleration, continuity advocates push for permanence—and the state must
 *   actively enforce the sunset clause against both pressures.
 *
 * KEY AGENTS:
 *   - State apparatus: Administers dual-script policy, enforces sunset clause, bears dual infrastructure costs
 *   - Older educated population: Literate in Arabic script, benefits from preservation of credentials and heritage access
 *   - Younger generations: Face dual-literacy burden, constrained by educational mandate, delayed standardization
 *   - Educators: Maintain dual-script competency, bear resource costs, constrained from either pure specialization
 *   - Secular nationalist bloc: Advocates for rapid Latin standardization, views coexistence as delay of modernization agenda
 *   - Ottoman continuity advocates: Would argue for permanent or indefinite coexistence, excluded by sunset clause
 *   - Religious/cultural institutions: Benefit from legitimized use of Arabic script, constrained by state modernization pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, 0.62).
domain_priors:suppression_score(turkish_graphemic_substrate__gradual_transition_reading, 0.58).
domain_priors:theater_ratio(turkish_graphemic_substrate__gradual_transition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__gradual_transition_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__gradual_transition_reading, scaffold).
narrative_ontology:human_readable(turkish_graphemic_substrate__gradual_transition_reading, "Turkish Dual-Script Literacy Transition (Gradual Reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__gradual_transition_reading, "political/linguistic/cultural").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:has_sunset_clause(turkish_graphemic_substrate__gradual_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__gradual_transition_reading, '35c34d1a-7014-40c5-95c0-843a0fca655b').
narrative_ontology:cs_kernel_codification('35c34d1a-7014-40c5-95c0-843a0fca655b', formalized).
narrative_ontology:cs_authority_grounding('35c34d1a-7014-40c5-95c0-843a0fca655b', extraction).
narrative_ontology:cs_interpretation_layer_present('35c34d1a-7014-40c5-95c0-843a0fca655b').
narrative_ontology:cs_reading_relation('35c34d1a-7014-40c5-95c0-843a0fca655b', turkish_graphemic_substrate__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('35c34d1a-7014-40c5-95c0-843a0fca655b', turkish_graphemic_substrate__secular_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('35c34d1a-7014-40c5-95c0-843a0fca655b', foundational, dual_script_coexistence_legitimacy).
narrative_ontology:cs_axiom_status(dual_script_coexistence_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('35c34d1a-7014-40c5-95c0-843a0fca655b', dual_script_coexistence_legitimacy, instrumental).
narrative_ontology:cs_axiom('35c34d1a-7014-40c5-95c0-843a0fca655b', foundational, temporal_bound_modernization_principle).
narrative_ontology:cs_axiom_status(temporal_bound_modernization_principle, holdable).
narrative_ontology:cs_axiom_grounding('35c34d1a-7014-40c5-95c0-843a0fca655b', temporal_bound_modernization_principle, conventional).
narrative_ontology:cs_reference_frame('35c34d1a-7014-40c5-95c0-843a0fca655b', managed_graphemic_transition_framework).
narrative_ontology:cs_drift_state('35c34d1a-7014-40c5-95c0-843a0fca655b', contemporary_state_modernization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('35c34d1a-7014-40c5-95c0-843a0fca655b', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, intergenerational_knowledge_holders).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, transition_administrators).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, educators_capable_of_dual_instruction).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, younger_generations_delayed_standardization).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, monolingual_latin_script_advocates).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, state_modernization_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, older_educated_population).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, younger_generations).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, religious_and_cultural_institutions).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__gradual_transition_reading, transition_planners_and_linguists).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, younger_generations).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, educators).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, secular_nationalist_bloc).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__gradual_transition_reading, religious_and_cultural_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the dual-script transition policy and enforces the sunset clause. Must maintain both scripts in public education, legal documents, and administrative infrastructure during the 5-15 year window. Bears the ongoing cost of maintaining dual literacy infrastructure while facing pressure from nationalist modernizers to accelerate Latin standardization. Cannot exit without either collapsing the arrangement or violating the sunset commitment.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, state_apparatus, agenda_setter,
    institutional, generational, trapped, national).

% Educated in Ottoman Arabic script; benefits from the coexistence arrangement as it preserves their literacy credentials and intellectual inheritance. Can read and produce documents in both scripts. Has high exit options: can choose to use either script socially, professionally, or intellectually without constraint. Possesses cultural capital anchored in the Arabic-script tradition.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, older_educated_population, beneficiary,
    powerful, biographical, arbitrage, national).

% Must achieve literacy in both scripts during the transition period, doubling the instructional burden and delaying achievement of standardized Latin-script fluency. Benefits incidentally from reduced intergenerational rupture and access to intellectual heritage texts, but the primary experience is constraint—additional years of dual-script instruction that monolingual schemes would not impose. Exit options constrained because educational standards require both scripts during the window.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, younger_generations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, younger_generations, beneficiary).

% Must teach both scripts and maintain dual-script literacy competency. Face resource constraints (dual materials, training in both systems, assessment frameworks for each script). Some educators benefit from employment continuity—those trained in Ottoman methods retain professional utility—but collectively the teaching force bears the coordination cost of maintaining two parallel systems. Constrained exit because state mandate requires dual-script instruction.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, educators, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, educators, agenda_setter).

% Advocates for rapid, complete transition to Latin script as the marker of modern national identity distinct from Ottoman-Islamic past. Views the dual-script coexistence as a compromise that delays their core agenda—the symbolic rupture from Ottoman civilization and alignment with European modernity. Constrained from simply abandoning the arrangement because state commitment to the sunset clause binds them to the transition period; they must lobby and wait rather than implement immediately.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, secular_nationalist_bloc, payer,
    powerful, generational, constrained, national).

% Would argue for permanent or indefinite dual-script coexistence, or even Arabic-script primacy as the legitimate substrate of Turkish linguistic identity. Structurally excluded from the coexistence arrangement because the state-mandated sunset clause is not negotiable—the very framework of the transition assumes eventual Latin standardization. Identity-locked because their claim to legitimacy is anchored in Ottoman-Islamic continuity, which the state's framework explicitly denies.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, ottoman_continuity_advocates, excluded,
    moderate, generational, identity_locked, national).

% Benefit from the dual-script coexistence as it permits continued use and transmission of Arabic-script texts (Quran, classical Islamic scholarship, Ottoman literary heritage). Simultaneously constrained by state pressure to participate in Latin-script standardization agenda. The arrangement permits private use of Arabic script in religious contexts while the public institutional trajectory moves toward Latin; this compartmentalization is both protective and confining.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, religious_and_cultural_institutions, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__gradual_transition_reading, religious_and_cultural_institutions, payer).

% Professional and intellectual class whose careers are enabled by the transition framework itself. Gain employment, status, and intellectual authority from administering, studying, and refining the dual-script coexistence system. Have relatively mobile exit options—they could shift to either nationalist-accelerationist or continuity-preservationist positions as institutional winds shift—but currently benefit from the managed-transition framing as a middle path that justifies their professional roles.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, transition_planners_and_linguists, beneficiary,
    organized, generational, mobile, national).

% Scholars, linguists, and policy analysts from outside Turkey examining the constraint as a case study in script transition, cultural engineering, and state capacity. No direct interest in the outcome; positioned to measure whether the dual-script coexistence achieves its stated goals (knowledge transfer, reduced generational rupture) or generates unintended extraction (state capacity diverted, institutional complexity bloat, symbolic bifurcation of national identity).
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__gradual_transition_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__gradual_transition_reading, diffuse).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__gradual_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the transition from one graphemic system (Arabic script, associated with Ottoman past) to another (Latin script, associated with modern European alignment) while preserving intergenerational knowledge transfer. Solves the coordination problem of maintaining accessibility to historical and cultural texts while enabling standardized modernization without complete rupture.
% TRANSFER_FUNCTION: Redirects educational resources and intergenerational time from what would be rapid standardization (Latin-script monolingual system) into maintenance of parallel literacy infrastructure. Moves opportunity costs from younger generations to state apparatus and educators, who bear the burden of administering dual systems. Also transfers cultural authority from secular-nationalist modernizers to Ottoman-continuity constituencies by legitimizing Arabic-script literacy during the transition window.
% ABSENT_VOICES: Ottoman-continuity advocates are structurally excluded by the sunset clause: they would argue for indefinite or permanent dual-script coexistence but are not seated at the transition-governance table because the state framework assumes eventual Latin standardization. Advocates for complete Arabic-script retention are similarly positioned outside the arrangement. Their objections, if present, would target the temporality itself—they would argue the constraint should have no sunset and that the gradual reading is a cover for cultural erasure.
% DISAPPEARANCE_RATIONALE: If the dual-script coexistence constraint and its 5-15 year transition window disappeared and were replaced by immediate Latin-script monolingual standardization, educational curricula would consolidate within years, younger generations would not face dual-literacy burden, state apparatus could reduce administrative complexity, and the intellectual landscape would bifurcate more sharply—older texts would be inaccessible to those without specialized training. Conversely, if the constraint disappeared into permanent Arabic-script retention (the continuity reading), the state's modernization project would stall, institutional standardization would fragment, and Ottoman-Islamic cultural authority would be reinstated in the graphemic system itself. The world does not hold the constraint in neutral equilibrium; the disappearance triggers reorganization in one direction or the other.
% FOUNDING_PROBLEM: After Ottoman institutional collapse, the Turkish state faced a legitimacy and modernization crisis: how to align the nation symbolically with European modernity (Latin script as marker) while preserving access to centuries of Ottoman-Islamic intellectual, legal, and cultural heritage (texts in Arabic script). Rapid, complete transition would maximize symbolic rupture but create generational rupture and render extant texts unintelligible to most of the population. The dual-script transition reads the founding problem as requiring both modernization AND knowledge continuity, solved through a managed, time-bounded coexistence.
% FOUNDING_PROBLEM_CORROBORATION: The secular-nationalist reading attests the founding problem was purely modernization—the need to align Turkey with European identity—and that the Arabic-script burden was obstacle, not heritage to preserve; they attest the problem is solved once Latin dominates. The Ottoman-continuity reading attests the founding problem was existential rupture and cultural erasure, and that the gradual reading does not adequately preserve continuity. Transition planners and linguists attest from their analytical seat that the dual-literacy burden is real and the knowledge-transfer benefit is measurable but modest. No single voice outside the transition-governance apparatus has corroborated the specific claim that 5-15 years is the 'right' duration; the sunset clause is a state choice, not an empirically grounded finding.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__gradual_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__gradual_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__gradual_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__gradual_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__gradual_transition_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__gradual_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__gradual_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.62) reflects that the constraint imposes real costs—dual-literacy instruction, dual administrative infrastructure, slowed standardization—that primarily fall on younger generations and the state apparatus. These costs are not fully justified by the coordination benefit; a unified Latin system would deliver faster standardization. However, extractiveness is not higher (not 0.75+) because the knowledge-transfer benefit is genuine and substantial—access to Ottoman heritage texts, intellectual continuity, reduced intergenerational rupture—which some constituencies (older educated population, religious institutions, transition scholars) actually value. The theater_ratio (0.48) reflects that dual-script administration is performed as a genuine transition framework, not theatrical performance, but begins to rise mid-interval (t=12) as the sunset approach intensifies pressure to prove the arrangement's value. The rising trajectory indicates the constraint's performative dimensions increase over time—more rhetoric about successful transition, more need to justify continued dual costs. Suppression_requirement rises over the interval (0.42 to 0.58) because as the sunset deadline approaches, both secular-nationalist accelerationists and continuity advocates intensify pressure, forcing the state to more actively police the transition's timeline and prevent either premature collapse or permanent embedding. The measurements share one time grid (t ∈ {0,3,6,9,12,15}) so the engine can sample all metrics at every point.
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus and transition planners experience the constraint as a SCAFFOLD—a legitimate, temporary coordination mechanism solving a real problem. Younger generations and the secular-nationalist bloc experience it as a TANGLED ROPE—their modernization is delayed, their state resources diverted to infrastructure they view as unnecessary. Ottoman continuity advocates experience it as a SNARE—they are excluded from the transition governance by the sunset clause itself, yet must live under its constraints. Older educated populations and religious institutions experience it as a ROPE—genuine coordination benefit (preserved literacy, heritage access) with modest per-capita cost. From the state's position (powerful, trapped in the framework, time-bound by the sunset), the arrangement is necessary coordination. From the younger generations' position (moderate power, constrained by dual-literacy mandate), it is extraction. From the continuity advocates' position (excluded, identity-locked), it is enforced erasure disguised as compromise. The engine computes these per-seat divergences from the structural data (power, exit options, beneficiary/victim status).
 *
 * DIRECTIONALITY LOGIC:
 *   State apparatus: d ≈ 0.45 (moderate beneficiary—benefits from the coordination it administers, but bears costs of maintaining two systems; not trapped enough to be a pure target, but constrained enough to be paying substantially). Older educated: d ≈ 0.15 (strong beneficiary—credentials preserved, heritage accessible, arbitrage exit options mean they can choose scripts; low extraction). Younger generations: d ≈ 0.72 (strong target—dual-literacy burden imposed, constrained exit, no compensating benefit perceived at their life stage; high extraction). Educators: d ≈ 0.60 (mixed target—resource burden is real, but employment continuity for dual-trained educators and professional status as transition administrators provides some offset). Secular nationalists: d ≈ 0.68 (target—their agenda delayed, state resources diverted away from their priority, exit constrained by democratic process rather than structural barriers). Ottoman continuity advocates: d ≈ 0.88 (strong target—excluded from governance, identity-locked, imposed constraint they oppose, no exit). Religious institutions: d ≈ 0.35 (mixed—compartmentalized access to Arabic script is protective for them, but constrained by state modernization pressure and sunset clause). The high directionality variance across seats (0.15 to 0.88) is what produces strong per-seat type divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is CONTESTED (does the state modernization problem require cultural rupture or can it accommodate continuity?), which prevents a clear mandatrophy verdict from the problem statement alone. However, the disappearance_verdict is WORLD_REARRANGES, indicating the constraint is NOT a mountain—it has arrangers and dependents, not just emergent natural fact. If the transition window is truly temporary (the sunset clause is enforced), then the constraint becomes PITON at t>15: the dual-script infrastructure was meant to decompose at the sunset, but cultural and educational constituencies may resist, maintaining performative dual-literacy requirements even after Latin standardization formally completes. The theater_ratio trajectory (rising in mid-interval, plateauing at 0.48) suggests the constraint is already experiencing some creep toward performative maintenance—more rhetoric about transition success, more institutional justification for continued costs. At the sunset boundary, if dual scripts persist due to institutional inertia (teachers trained in both, materials in both, cultural resistance to complete erasure), the mandatrophy would be confirmed: the founding coordination problem is solved (Latin standardization has succeeded), but the constraint persists theatrically. The constraint's integrity depends on ENFORCING the sunset clause; if the state abandons the sunset, it becomes a permanent snare (younger generations locked into dual-literacy indefinitely).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sunset_enforcement_ambiguity,
    'Will the state actually enforce the sunset clause at t=5-15, or will institutional, cultural, and educational inertia cause the dual-script system to persist indefinitely?',
    'Historical observation at the sunset boundary: if Latin-script monolingualism is imposed and dual-script literacy is discontinued in curricula and administration, the sunset was enforced; if dual-script coexistence continues beyond the declared window with renewed justifications, the sunset was not enforced.',
    'If the sunset is enforced, the constraint is genuinely SCAFFOLD—temporary coordination that decomposes at its declared end. If the sunset is not enforced, the constraint transitions to PITON (performatively maintained, coordinating no longer, persisting through inertia) or potentially back to SNARE (if younger generations continue to be burdened without the transition rationale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_enforcement_ambiguity, empirical, 'Whether the managed transition is genuinely temporary or becomes permanent through institutional drift.').

omega_variable(
    knowledge_transfer_quantification,
    'What is the actual magnitude of knowledge-transfer loss if the transition is accelerated to Latin-only, and does it justify the dual-literacy burden imposed on younger generations?',
    'Comparative study of cohorts educated in dual-script vs. Latin-only systems: measure intergenerational access to Ottoman-era texts, comprehension of intellectual heritage, cultural continuity markers. Compare cost of instruction (dual-literacy burden) to value of heritage access.',
    'If knowledge transfer is substantial and valued, the constraint''s extraction is justified as coordination cost. If transfer is minimal or not valued by target populations, the extraction is unjustified and the constraint should be reclassified upward in extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_transfer_quantification, empirical, 'The actual benefit of intergenerational knowledge preservation relative to the burden imposed.').

omega_variable(
    reading_kernel_foreclosure,
    'Do the three readings (ottoman_continuity, secular_nationalist, gradual_transition) logically coexist within Turkish state commitment, or does commitment to one reading foreclose others?',
    'Examine whether the state''s official position permits all three readings or has formally rejected some. Test whether different political constituencies simultaneously hold incompatible readings without internal contradiction.',
    'If readings coexist, the gradual reading is a compromise between incompatible constituencies (coexists_with relation). If the gradual reading forecloses or is foreclosed by siblings, the kernel''s structure is more rigidly hierarchical and the gradual reading''s legitimacy depends on which reading the state enforces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether the three kernel readings are logically compatible within the state''s foundational commitments.').

omega_variable(
    generational_rupture_internalization,
    'Is the measured ''intergenerational rupture'' from rapid Latinization structural (external, easily reversible at later generations) or internalized (the younger generation internalizes Latin-only identity, making Arabic literacy psychologically foreign)?',
    'Post-exit analysis: measure whether individuals who learn dual-script literacy during the transition maintain Arabic-script competency and identity-connection after the constraint ends, or whether they shed Arabic literacy once it is no longer mandated. Compare to cohorts raised in Latin-only systems who later attempt to learn Arabic script.',
    'If internalized, the dual-literacy system''s benefit is real—it prevents identity fusion with Latin-only modernity that would make Arabic recovery difficult. If structural only, the benefit is overstated and younger generations bear unnecessary burden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_rupture_internalization, empirical, 'Whether intergenerational rupture is preventable through the transition or is a consequence of state identity choices that persists regardless.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__gradual_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(turk_tr_t3, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 3, 0.4).
narrative_ontology:measurement(turk_tr_t6, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(turk_tr_t9, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 9, 0.48).
narrative_ontology:measurement(turk_tr_t12, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(turk_tr_t15, turkish_graphemic_substrate__gradual_transition_reading, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(turk_be_t3, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(turk_be_t6, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement(turk_be_t9, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 9, 0.62).
narrative_ontology:measurement(turk_be_t12, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(turk_be_t15, turkish_graphemic_substrate__gradual_transition_reading, base_extractiveness, 15, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(turk_su_t3, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(turk_su_t6, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 6, 0.54).
narrative_ontology:measurement(turk_su_t9, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(turk_su_t12, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(turk_su_t15, turkish_graphemic_substrate__gradual_transition_reading, suppression_requirement, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__gradual_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__gradual_transition_reading, 0.12).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__gradual_transition_reading, turkish_graphemic_substrate__secular_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint (gradual_transition_reading) is one of three readings of the turkish_graphemic_substrate kernel. The three readings differ fundamentally in how they interpret the legitimacy of script choice: ottoman_continuity_reading asserts Arabic-script permanence as culturally legitimate; secular_nationalist_reading asserts Latin-script monopoly as modernization marker; gradual_transition_reading asserts temporary coexistence as solving both modernization and continuity problems. Each reading has distinct beneficiary/victim structure and extraction profile. The network links capture that commitment to one reading constrains the viability of others—the state cannot simultaneously enforce the gradual transition and the immediate nationalist transition (incompatible deadlines), though it can navigate between them through time. The epsilon values differ substantially across readings because the referent is the STANDING ARRANGEMENT UNDER CONTEST in each reading: gradual reading's referent is the coexistence arrangement (extractive because it imposes dual burden); nationalist reading's referent is rapid Latin standardization (extractive because it erases heritage); continuity reading's referent is permanent Arabic retention (extractive because it blocks modernization from the state's perspective). Do not merge these into one constraint with measurement basis—they are fundamentally different instantiations of the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__gradual_transition_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
