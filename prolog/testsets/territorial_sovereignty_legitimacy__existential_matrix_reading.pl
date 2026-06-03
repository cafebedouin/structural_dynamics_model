% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Existential Matrix Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the EXISTENTIAL MATRIX READING of the
 *   territorial sovereignty legitimacy kernel. The reading asserts that
 *   legitimacy claims grounded in covenant history (Balfour Declaration,
 *   ancient residence, UN recognition) or self-determination principles
 *   (demographic majority, national self-governance) are epiphenomenal — they
 *   rationalize but do not drive territorial conflict. The actual driver is
 *   existential: each people requires exclusive territorial control as a
 *   non-negotiable precondition for collective survival, demographic
 *   reproduction, and identity expression. Under this reading, the conflict
 *   is fundamentally zero-sum regardless of juridical or historical
 *   arguments. Compromise frameworks (two-state solutions, shared governance,
 *   territorial swaps) fail structurally because neither side can accept the
 *   vulnerability that comes with territorial partition or power-sharing. The
 *   beneficiary is whichever side achieves military and demographic
 *   dominance; the victims are the territorially displaced population and the
 *   weaker sovereignty claimant. The constraint operates through existential
 *   fear (suppression = 0.82) rather than legal prohibition. Legal-diplomatic
 *   frameworks (UN law, international humanitarian law, peace process
 *   architecture) are largely performative (theater_ratio = 0.55) — they
 *   absorb institutional energy without changing the underlying existential
 *   driver. The extractiveness has risen over the measurement interval (0.52
 *   → 0.74) as military dominance has consolidated and displacement has
 *   deepened, indicating that the constraint's extraction mechanism
 *   strengthens as the dominant side's control becomes more absolute.
 *   Suppression has similarly intensified (0.65 → 0.82) as security apparatus
 *   and demographic control mechanisms have expanded. Theater ratio has
 *   declined (0.68 → 0.55) as legal-diplomatic activity has become
 *   increasingly decoupled from actual territorial control dynamics, leaving
 *   less performative cover over the underlying extraction.
 *
 * KEY AGENTS:
 *   - Militarily Dominant Side: Primary beneficiary (institutional/arbitrage) — captures territorial control and demographic advantage; experiences constraint as coordination mechanism (boundary maintenance, survival necessity)
 *   - Displaced Population: Primary victim (powerless/trapped) — bears full extraction cost of territorial control and identity erasure; no exit option available
 *   - Minority Sovereignty Claimant: Secondary victim (moderate/constrained) — constrained by military imbalance and existential fear of own displacement; extraction mechanism: structural impossibility of compromise when both sides frame territory as existential necessity
 *   - Compromiser Coalition: Organized actor (organized/constrained) — international peace-process architects, regional moderates who perceive genuine coordination function in compromise but are structurally prevented from implementing it by existential framing
 *   - Legal-Historical Framework: Institutional theater (analytical/analytical) — international law and self-determination doctrine attempt juridical adjudication but function performatively because existential drivers override legal legitimacy claims
 *   - Analytical Observer (Civilizational): Risk of naturalizing contingent institutional arrangement as inevitable zero-sum law of group identity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.82).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty Legitimacy (Existential Matrix Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '817debd3-4fc6-468b-90c3-4560792280f1').
narrative_ontology:cs_kernel_codification('817debd3-4fc6-468b-90c3-4560792280f1', distributed).
narrative_ontology:cs_authority_grounding('817debd3-4fc6-468b-90c3-4560792280f1', extraction).
narrative_ontology:cs_reading_relation('817debd3-4fc6-468b-90c3-4560792280f1', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('817debd3-4fc6-468b-90c3-4560792280f1', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_axiom('817debd3-4fc6-468b-90c3-4560792280f1', foundational, territory_existential_necessity).
narrative_ontology:cs_axiom_status(territory_existential_necessity, holdable).
narrative_ontology:cs_axiom_grounding('817debd3-4fc6-468b-90c3-4560792280f1', territory_existential_necessity, empirically_contingent).
narrative_ontology:cs_axiom('817debd3-4fc6-468b-90c3-4560792280f1', foundational, legitimacy_epiphenomenal).
narrative_ontology:cs_axiom_status(legitimacy_epiphenomenal, holdable).
narrative_ontology:cs_axiom_grounding('817debd3-4fc6-468b-90c3-4560792280f1', legitimacy_epiphenomenal, empirically_contingent).
narrative_ontology:cs_reference_frame('817debd3-4fc6-468b-90c3-4560792280f1', existential_territorial_zero_sum).
narrative_ontology:cs_drift_state('817debd3-4fc6-468b-90c3-4560792280f1', contemporary_international_relations_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('817debd3-4fc6-468b-90c3-4560792280f1', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, militarily_dominant_side).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, territorially_displaced_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, minority_sovereignty_claimant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED POPULATION (SNARE) — Trapped by military occupation, refugee status, or legal exclusion from territorial claim. No exit option: return to ancestral territory is structurally blocked by the dominant side's existential claim that their survival requires exclusive control. Bears full extraction cost — territorial displacement and identity erasure. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MINORITY SOVEREIGNTY CLAIMANT (SNARE) — Constrained by military imbalance, international non-recognition, and existential fear of their own displacement. Exit options (diaspora relocation, acceptance of minority status) carry identity death and abandonment of survival claims. Extraction mechanism: structural impossibility of compromise when both sides frame territory as existential necessity. High experienced extractiveness despite moderate power.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPROMISER COALITION (TANGLED ROPE) — International actors, regional moderates, and peace-process architects see genuine coordination function: territorial compromise (two-state solution, land swaps, shared governance) would reduce conflict-driven resource depletion and enable development. But the constraint prevents cooperation — existential framing makes compromise appear as surrender rather than coordination gain. Moderate-high extraction (negotiation stalling, peace-process costs) alongside genuine coordination potential that remains structurally unavailable.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MILITARILY DOMINANT SIDE (ROPE) — Perceives the constraint as coordination mechanism: territorial control enables collective survival, identity expression, and demographic reproduction. The 'snare' experienced by the displaced is experienced as necessary boundary maintenance by the dominant side. Net beneficiary — experiences constraint as positive coordination (defensive necessity) rather than extraction. Experienced extractiveness is negative or near-zero from this perspective.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL-HISTORICAL OBSERVER (PITON) — International law frameworks (UN Partition Plan, international humanitarian law, self-determination doctrine, refugee law) attempt to adjudicate the conflict through juridical argument. But the framework is largely performative — legal settlement attempts fail because neither side accepts the legitimacy of law-based compromise when they frame the issue as existential. The constraint persists through legal theater while existential drivers remain unaddressed. Theater ratio reflects that legal-diplomatic activity is substantial but functionally inert relative to territorial control mechanisms.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVILIZATIONAL-EXISTENTIAL OBSERVER (MOUNTAIN) — From a civilizational scale, some conflicts are indeed zero-sum when parties frame territory as existential survival requirement. Collective identity requires territorial anchor; competing identities on the same territory create immutable structural opposition. This perspective sees the constraint as a natural law of group identity and survival. However, this classification is contestable — the engine's false summit detection may flag this as naturalization of a contingent institutional arrangement (existential framing as strategy rather than inevitable condition).
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__existential_matrix_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Under the existential reading, the dominant side extracts territorial control, demographic advantage, and identity expression security through military enforcement. The displaced population experiences maximum extraction (territorial loss, identity erasure). The minority claimant experiences high extraction (constrained sovereignty, vulnerability to displacement). The extractiveness value reflects that the mechanism operates through asymmetric existential gain for the dominant side and existential loss for the subordinate sides. Suppression (0.82): Very high. The constraint is enforced through military occupation, legal exclusion from territorial claim, demographic policies, and security apparatus targeting. Alternatives (minority cohabitation, power-sharing, return of displaced populations) are suppressed through combination of military force, legal prohibition, and existential fear that accepting alternatives equals accepting vulnerability to displacement. Theater ratio (0.55): Moderate-declining. Legal and diplomatic frameworks (UN law, peace processes, humanitarian law) generate substantial institutional activity but are increasingly decoupled from actual territorial control mechanisms. As military dominance has consolidated, the performative aspect of legal frameworks has declined — external observers no longer expect that juridical arguments will resolve the conflict, reducing the credibility of legal theater. The measurement trajectory (declining theater, rising suppression, rising extractiveness) indicates a constraint moving from hybrid coordination-extraction (when legal frameworks had some capacity to constrain military action) toward pure extraction mechanism (military dominance determining outcome regardless of legal or diplomatic intervention).
 *
 * PERSPECTIVAL GAP:
 *   The existential matrix reading generates extreme perspectival divergence. The dominant side experiences the constraint as positive coordination (Rope perspective: territorial control enables collective survival and identity). The displaced population experiences it as pure extraction (Snare: territorial loss, identity erasure, no exit). The minority claimant experiences it as mixed constraint (Snare: military dominance blocks sovereignty; identity extinction risk prevents compromise). The compromiser sees structural impossibility of cooperation (Tangled Rope: genuine coordination potential but existential framing prevents realization). The legal framework sees its own irrelevance (Piton: substantial institutional activity with declining functional capacity). The civilizational observer risks false summit (Mountain: zero-sum as inevitable law of group identity). The gap reflects that the constraint's operation depends entirely on existential framing — how each agent interprets territorial control as essential or negotiable determines whether they experience it as coordination or extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality and effective extractiveness chi derives from structural position in the existential matrix: (1) The displaced population (powerless/trapped) has d ≈ 0.95, producing f(d) ≈ 1.42 — maximum experienced extraction. (2) The minority claimant (moderate/constrained) has d ≈ 0.75, producing f(d) ≈ 1.05 — high experienced extraction despite moderate power, because exit options are constrained by existential fear of displacement and lack of military capacity. (3) The dominant side (institutional/arbitrage) has d ≈ 0.10, producing f(d) ≈ -0.05 — negative or near-zero experienced extraction; they experience the constraint as boundary maintenance and coordination mechanism, not as extraction. (4) The compromiser coalition (organized/constrained) has d ≈ 0.55, producing f(d) ≈ 0.75 — moderate-high experienced extraction because compromise requires accepting existential risk that the coalition cannot unilaterally manage; they bear the cost of proposal and failure. (5) The legal-historical framework (analytical/analytical) has canonical d ≈ 0.72, but the piton classification indicates that the juridical framework is largely inert — its directionality operates at a decoupled level from actual territorial power dynamics. (6) The civilizational-existential observer (analytical/analytical) risks false-summit classification by naturalizing the existential zero-sum as inherent rather than as a contingent institutional framing choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The existential matrix reading resolves the mandatrophy by asserting that legitimacy claims are epiphenomenal to the actual existential driver. From a covenant continuity reading, legitimacy would be grounded in ancient history and continuous presence; from a self-determination reading, legitimacy would be grounded in modern democratic principles applied to demographic majority. But the existential reading collapses both: legitimacy claims matter only insofar as they motivate groups to defend territories they already frame as existentially necessary. The dominant side experiences Rope (coordination) because they win the existential competition; the displaced population experiences Snare because they lose it. The mandatrophy resolution is: all six types are real, but they map to asymmetric outcomes in which the existential competition determines winner and loser, and winner and loser determine whether the same constraint appears as coordination or extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_framing_versus_strategic_choice,
    'Is the existential framing (territory as non-negotiable survival requirement) an inevitable fact of group psychology, or a strategic choice adopted by political elites to prevent compromise?',
    'Historical comparison: cases where territorially displaced populations accepted minority status (Huguenots, Igbo diasporas, Armenian diaspora communities); cases where groups negotiated shared or rotating territorial control; analysis of how existential rhetoric emerges and is deployed in conflict escalation.',
    'If inevitable fact: classification remains Snare at all contexts; zero-sum is immutable. If strategic choice: classification reverts to Tangled Rope at moderate power context; compromise becomes structurally possible when existential framing is denormalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_framing_versus_strategic_choice, empirical, 'Whether existential framing is inevitable or strategically deployed').

omega_variable(
    demographic_control_versus_identity_legitimacy,
    'Does military/demographic control of territory constitute the actual legitimacy source, or does it merely enforce a claim that is validated by prior juridical/covenant arguments?',
    'Reverse counterfactual: if the militarily dominant side lost control, would they renounce their legitimacy claim? Historical precedent (post-WWII transfers of sovereignty, UN decolonization outcomes). Analysis of separatist movements where legitimacy claims persist despite military defeat.',
    'If control is source: constraint is purely structural (Snare, extraction by military dominance). If control enforces claim: constraint is hybrid (Tangled Rope, mixing legitimate claim with illegitimate enforcement mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_control_versus_identity_legitimacy, conceptual, 'Whether control creates legitimacy or enforces it').

omega_variable(
    third_party_enforceability_of_compromise,
    'Can external powers (superpowers, UN, regional hegemon) enforce territorial compromise against existential resistance, or does external enforcement itself become part of the extraction mechanism?',
    'Case analysis of imposed partition settlements (India-Pakistan, Korea, Vietnam, Mandated Territories under League of Nations); measurement of stability duration and reversion likelihood when external enforcement weakens.',
    'If enforceable: compromiser coalition (Perspective 3) could stabilize compromise framework and convert Snare to Tangled Rope. If unenforceable: external intervention adds layer of extraction (external power benefits from resource deployment), converting to more complex Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_enforceability_of_compromise, empirical, 'Whether third-party enforcement can stabilize territorial compromise').

omega_variable(
    reading_committer_kernel_contest,
    'Which reading of the territorial sovereignty legitimacy kernel accurately describes the actual driver of conflict: existential matrix (this reading), covenant continuity, or self-determination principle?',
    'This omega documents the irreducible committer-level frame selection. The three readings (existential_matrix_reading, covenant_continuity_reading, self_determination_reading) represent different authority structures grounding legitimacy claims. Resolution requires normative choice, not empirical evidence. The engine routes this to the committer manifest for explicit political choice rather than algorithmic adjudication.',
    'Reading choice determines: (1) who is beneficiary vs victim in the structural classification; (2) whether legitimacy is epiphenomenal (existential reading) or foundational (covenant/self-determination readings); (3) whether compromise is structurally impossible (existential) or merely difficult (covenant/self-determination); (4) which omega questions are diagnostically relevant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_kernel_contest, preference, 'Kernel reading selection: existential matrix vs covenant continuity vs self-determination').

omega_variable(
    false_summit_mountain_risk,
    'Does the mountain classification (Perspective 6) naturalize as inevitable what is actually a contingent institutional or framing choice? Is existential zero-sum territorial conflict inherent to human group dynamics, or does it depend on specific historical and discursive conditions?',
    'Historical and anthropological evidence: prevalence of non-territorial identity systems (religious communities, professional guilds, diaspora networks) that maintain collective identity without territorial anchor; evidence of successful identity maintenance without exclusive territorial control; cases of multiple groups cohabiting same territory with stable identity boundaries.',
    'If naturalization detected: constraint reclassifies to Tangled Rope (the existential framing is institutional enforcement mechanism, not natural law); beneficiary-victim structure becomes contingent on political strategy rather than inevitable. If natural law confirmed: Snare and Mountain classifications stand; zero-sum is immutable structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_mountain_risk, empirical, 'Whether existential territorial zero-sum is natural law or contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_sov_exist_theater_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(terr_sov_exist_theater_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 25, 0.6).
narrative_ontology:measurement(terr_sov_exist_theater_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(terr_sov_exist_extract_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(terr_sov_exist_extract_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(terr_sov_exist_extract_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(terr_sov_exist_suppress_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(terr_sov_exist_suppress_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(terr_sov_exist_suppress_t50, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 50, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, two_state_solution_structural_instability).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_based_resource_allocation).

% DUAL FORMULATION NOTE:
% The territorial sovereignty legitimacy kernel decomposes into three competing readings, each with different ε values and structural implications. The existential matrix reading (this story, ε=0.68) asserts that legitimacy claims are epiphenomenal and compromise is structurally impossible. The covenant continuity reading (ε≈0.45-0.55, Tangled Rope) grounds legitimacy in historical and legal claims that might accommodate legal compromise. The self-determination reading (ε≈0.45-0.55, Tangled Rope) grounds legitimacy in modern democratic principle that might accommodate proportional representation or equal sovereignty. These are distinct constraints with different classification architectures. The existential reading links downstream to the two-state solution instability constraint (which models why compromise frameworks fail) and to identity-coordination constraints (which model how existential framing operates through group identity mechanisms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
