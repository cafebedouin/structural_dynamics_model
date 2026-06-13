% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__state_centric_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__state_centric_reading
 *   human_readable: Combatant Status Definition (State-Centric Reading)
 *   domain: legal/political/humanitarian
 *
 * SUMMARY:
 *   The state-centric reading of combatant status is one interpretation of a
 *   contested kernel in international humanitarian law. It holds that
 *   combatant status—and with it, Geneva Convention protections, immunity
 *   from prosecution for lawful combat, and prisoner-of-war rights—is
 *   available ONLY to members of state armed forces that meet Article 4 of
 *   the Third Geneva Convention: responsible command, fixed distinctive
 *   insignia, arms openly carried, and compliance with laws of war. Non-state
 *   armed groups, regardless of organizational sophistication or humanitarian
 *   compliance, are categorically excluded from combatant status and fall
 *   into a legal gray zone where they are neither protected as prisoners of
 *   war nor prosecuted as regular criminals. This reading benefits state
 *   militaries and hegemonic state actors; it extracts from non-state
 *   fighters and from detainees classified outside the status binary.
 *
 * KEY AGENTS:
 *   - state_militaries: structural beneficiaries of the constraint, receive full Geneva protections and immunity
 *   - hegemonic_state_actors: agenda setters who authored and enforce the state-centric interpretation through treaty bodies and military retaliation
 *   - non_state_armed_groups: primary targets of the constraint, denied combatant status despite organizational capacity, subject to domestic prosecution
 *   - liberation_fighters: secondary targets, categorically excluded by the formal criterion despite resistance legitimacy claims
 *   - detainees_outside_status: powerless victims, subject to legal black-hole detention without POW protections or transparent trial
 *   - humanitarian_organizations: dual role—benefit from clear combatant/civilian distinction but constrained by inability to extend protections outside the binding rule
 *   - international_legal_scholars: excluded from hegemonic interpretation bodies, advocate for functional or AP I readings
 *   - domestic_legal_systems: observers applying the state-centric rule by default, some beginning to shift toward functional readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, 0.79).
domain_priors:suppression_score(combatant_status_definition__state_centric_reading, 0.81).
domain_priors:theater_ratio(combatant_status_definition__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(combatant_status_definition__state_centric_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__state_centric_reading, "Combatant Status Definition (State-Centric Reading)").
narrative_ontology:topic_domain(combatant_status_definition__state_centric_reading, "legal/political/humanitarian").

domain_priors:requires_active_enforcement(combatant_status_definition__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__state_centric_reading, '5d200a3d-d764-4096-a951-562c217b9fbf').
narrative_ontology:cs_kernel_codification('5d200a3d-d764-4096-a951-562c217b9fbf', formalized).
narrative_ontology:cs_authority_grounding('5d200a3d-d764-4096-a951-562c217b9fbf', extraction).
narrative_ontology:cs_interpretation_layer_present('5d200a3d-d764-4096-a951-562c217b9fbf').
narrative_ontology:cs_reading_relation('5d200a3d-d764-4096-a951-562c217b9fbf', combatant_status_definition__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('5d200a3d-d764-4096-a951-562c217b9fbf', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('5d200a3d-d764-4096-a951-562c217b9fbf', foundational, state_monopoly_on_legitimate_combatant_status).
narrative_ontology:cs_axiom_status(state_monopoly_on_legitimate_combatant_status, holdable).
narrative_ontology:cs_axiom_grounding('5d200a3d-d764-4096-a951-562c217b9fbf', state_monopoly_on_legitimate_combatant_status, conventional).
narrative_ontology:cs_axiom('5d200a3d-d764-4096-a951-562c217b9fbf', foundational, formal_organization_criterion_sufficient_for_clarity).
narrative_ontology:cs_axiom_status(formal_organization_criterion_sufficient_for_clarity, overridden).
narrative_ontology:cs_axiom_grounding('5d200a3d-d764-4096-a951-562c217b9fbf', formal_organization_criterion_sufficient_for_clarity, empirically_contingent).
narrative_ontology:cs_reference_frame('5d200a3d-d764-4096-a951-562c217b9fbf', formal_state_combatant_monopoly).
narrative_ontology:cs_drift_state('5d200a3d-d764-4096-a951-562c217b9fbf', contemporary_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5d200a3d-d764-4096-a951-562c217b9fbf', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__state_centric_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, hegemonic_state_actors).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, liberation_fighters).
narrative_ontology:constraint_victim(combatant_status_definition__state_centric_reading, detainees_outside_status).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__state_centric_reading, humanitarian_organizations).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, state_monopoly_on_legitimate_force).
narrative_ontology:constraint_vindicates(combatant_status_definition__state_centric_reading, formal_hierarchy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State armed forces that meet Article 4 criteria (responsible command, fixed insignia, arms openly carried, compliance with laws of war) automatically qualify for combatant status and full POW protections if captured. They benefit from the constraint's clarity: their members are protected by the Geneva Conventions, cannot be prosecuted for lawful combat, and are guaranteed humane treatment and fair trial if detained. The constraint's stringency toward non-state actors reinforces the state's monopoly on legitimate force.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, state_militaries, beneficiary,
    institutional, generational, arbitrage, national).

% Powerful states that authored and enforce the Geneva Conventions framework (primarily Western military establishments and their allied states). They set the definitional criteria for combatant status, control the International Court of Justice and treaty interpretation bodies, and have the military capacity to enforce compliance. They benefit from a definition that protects their own fighters while categorizing adversaries and non-state challengers as unlawful combatants subject to domestic law and potential war crimes prosecution.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, hegemonic_state_actors, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Armed organizations fighting state authority or occupation—whether nationalist movements, liberation groups, or insurgent forces—cannot qualify for combatant status under the state-centric reading regardless of organizational capacity or compliance with humanitarian norms. Their members are classified as unlawful combatants or terrorists, denying them POW status if captured. They face prosecution under domestic criminal law, torture risk, and absence of Geneva protections. Exit means abandoning the armed struggle entirely; they cannot 'reform' their way into status because the criterion is state-centricity.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, non_state_armed_groups, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, non_state_armed_groups, excluded).

% Non-state actors fighting against colonial occupation, racist regimes, or foreign military occupation claim combatant status under alternative readings and under customary international law in some jurisdictions. Under the state-centric reading, they are categorically excluded; their organizational legitimacy and humanitarian compliance cannot overcome the structural requirement of state affiliation. Their members carry the highest execution and prosecution risk.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, liberation_fighters, payer,
    moderate, biographical, identity_locked, regional).

% Individuals detained by state forces who fall outside combatant status are not entitled to POW protections or regular criminal trial proceedings in many jurisdictions. They occupy a legal black hole: neither protected as prisoners of war nor prosecuted under transparent criminal procedures. Common Article 3 minimum protections apply nominally, but enforcement is weak and state discretion is broad.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, detainees_outside_status, payer,
    powerless, immediate, trapped, local).

% The International Committee of the Red Cross and other humanitarian organizations benefit from a clear combatant/civilian distinction, which enables them to operate and advocate within a legible framework. However, they are also constrained by the constraint: they cannot extend full protections to non-combatant non-state actors without challenging the state-centric reading. They navigate a dual role: applying the binding rule while advocating for functional protections outside it.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, humanitarian_organizations, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__state_centric_reading, humanitarian_organizations, observer).

% Academic lawyers, particularly those from the Global South and liberation-focused traditions, advocate for functional or expansive readings of combatant status. They are excluded from treaty-drafting and interpretation at hegemonic legal forums. They would argue for AP I Article 1(4) applicability and functional equivalence criteria, but their alternative readings are marginalized in enforcement contexts.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, international_legal_scholars, excluded,
    organized, generational, constrained, global).

% National courts and prosecutors operate within the state-centric framework by default, applying it to prosecute non-state fighters under domestic terrorism, treason, or war crimes statutes. Some jurisdictions have begun to apply functional or humanitarian readings in their own case law, but international treaty bodies continue to reinforce the state-centric reading.
narrative_ontology:constraint_stakeholder(combatant_status_definition__state_centric_reading, domestic_legal_systems, observer,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__state_centric_reading, hegemonic_state_actors).
narrative_ontology:fixing_cost_class(combatant_status_definition__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binary classification system (combatant/non-combatant, lawful/unlawful) that simplifies the legal status of detained persons and clarifies when Geneva Conventions apply. Coordinates military conduct by distinguishing combatants (who may lawfully fight and are protected if captured) from civilians (who must not be targeted). The definition's formality—requiring state organization, command structure, insignia, and recognition—creates a legible marker that allows combatants to verify each other's status on the battlefield.
% TRANSFER_FUNCTION: Transfers POW protection, immunity from prosecution for lawful combat, and Geneva Convention protections FROM state militaries (who automatically receive status and must grant these protections to captured enemy combatants) TO state sovereignty and military hierarchy as institutions. Conversely, it transfers prosecution risk, torture vulnerability, and legal precarity FROM non-state fighters (who receive no protections) TO state control over who is deemed a legitimate combatant. The constraint moves legitimacy and legal immunity to state-affiliated actors and removes it from non-state challengers.
% ABSENT_VOICES: National liberation movements, insurgent groups, and their legal advocates would argue for functional criteria or AP I Article 1(4) applicability. Detainees classified as unlawful combatants have no voice in the interpretation of the constraint that excludes them. Academic and NGO voices from the Global South calling for expansive or humanitarian readings are structurally absent from hegemonic legal forums. Surviving non-state fighters who were denied POW status cannot testify before treaty bodies that drafted the original constraint.
% DISAPPEARANCE_RATIONALE: If the state-centric definition disappeared and all armed groups meeting functional criteria (command, organization, humanitarian compliance) received combatant status, the legal landscape would reorganize: non-state fighters would receive POW protections, immunity from domestic prosecution for lawful combat, and access to prisoner-exchange mechanisms. States would lose a tool for prosecuting non-state adversaries as criminals. Proxy conflicts would shift in character because sponsoring states could no longer hide behind unlawful combatant classifications. Humanitarian access to conflict zones would expand. The constraint's removal would not end armed conflict but would rebalance legal protections toward functional equality rather than institutional hierarchy.
% FOUNDING_PROBLEM: Post-WWII international order sought to prevent atrocities by clarifying combatant status—distinguishing soldiers (protected if captured) from civilians (protected from targeting). The state-centric criterion was meant to prevent bands of armed men from claiming combatant immunity by requiring formal organization, responsible command, and verifiable affiliation. The fear was that functional criteria alone would proliferate false combatants and make it harder to distinguish lawful combatants from terrorists.
% FOUNDING_PROBLEM_CORROBORATION: State military establishments and hegemonic legal authorities attest the founding problem is still live: non-state fighters may masquerade as combatants and exploit protections while targeting civilians. International humanitarian law scholars and human rights organizations attest the founding problem has been solved (organizational and command criteria are verifiable) and the constraint now functions as a tool for excluding legitimate resistance from protection. UN fact-finding missions and humanitarian bodies outside the treaty framework corroborate the shifted-function reading: the constraint now primarily protects state privilege, not civilians. Academic literature on AP I Article 1(4) and customary international law documents the contest explicitly.
narrative_ontology:disappearance_verdict(combatant_status_definition__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(combatant_status_definition__state_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.79) and rising over the 75-year interval because the constraint's primary function has shifted from clarifying battlefield norms (coordination) to enabling prosecution of non-state adversaries (extraction). The measurement series shows base_extractiveness rising from 0.48 to 0.79, with suppression_requirement rising from 0.64 to 0.81—a trajectory consistent with extraction accumulation. Theater_ratio rises from 0.22 to 0.42, indicating that a growing share of enforcement activity (military raids, detention reviews, prosecution arguments) defends the formal criterion rather than protecting civilians. The accessibility_collapse at the individual level (non-state fighters cannot access status through any route short of disbanding) is high and stable (0.65→0.70). Stakes_inflation at the individual level is severe (0.78→0.82): captured non-state fighters face domestic prosecution, torture risk, and execution. Suppression is actively applied: states prosecute fighters, exclude them from prisoner exchanges, and prevent alternative legal frameworks. Resistance is moderate (0.55→0.62 structural, 0.68→0.70 class) because liberation movements continue to fight and legal scholars contest the reading, but they lack enforcement power against hegemonic state practice. This is a tangled_rope because genuine coordination (battlefield clarity) coexists with asymmetric extraction (state protection + non-state denial). The claim/metric gap is intentional: the constraint is CLAIMED as rope (coordination function is real) while the authored metrics describe substantially extractive operation (the coordination function's share of enforcement is declining).
 *
 * PERSPECTIVAL GAP:
 *   From the state militaries' perspective, the constraint is pure coordination: it establishes clear rules that enable them to fight lawfully and be protected. From the non-state fighters' perspective, the same constraint is extraction: it denies them legal status based on a criterion (formality) they cannot meet without ceasing to exist. Hegemonic state actors experience the constraint as legitimate authority (they wrote it); Global South states and liberation scholars experience it as institutional imperialism (it was imposed on them). The engine computes these perspectives as divergent directionalities: state_militaries sit at d≈0.0-0.2 (beneficiaries), non_state_armed_groups sit at d≈0.85-1.0 (full targets). Humanitarian organizations sit near d≈0.5 (they benefit from clarity but are constrained by state-centricity). The divergence is structural, not observer-relative.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and hegemonic state actors are structural beneficiaries (d near 0.0): they set the rules, collect protections without meeting any burden of proof, and use the constraint to prosecute rivals. Their exit options are 'arbitrage'—they can switch to alternative legal readings if hegemonic rule erodes, or they can ignore the rule entirely if enforceability fails. Non-state armed groups are structural targets (d near 1.0): the constraint extracts legal precarity from them regardless of their organizational capability or humanitarian compliance. Their exit options are 'identity_locked'—they cannot exit by 'becoming more formal' because the criterion is state affiliation, not formality. They cannot exit by 'abandoning the constraint' because it is imposed by others. They can only exit by ceasing armed struggle. Humanitarian organizations sit symmetrically (d≈0.5): they benefit from the constraint's clarity but are harmed by its exclusions. Domestic legal systems are observers (d≈0.5-0.6): they apply the rule but some are beginning to contest it through functional readings. This structure generates the asymmetry: the beneficiary seats can modify the constraint if it becomes politically costly; the target seats cannot modify it even if enforcement becomes costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was battlefield clarity: enable combatants to verify legitimacy and civilians to understand who is a lawful target. The founding problem status is 'contested'—hegemonic state actors attest it is still live (non-state forces pose identification risks), while humanitarian bodies and legal scholars attest it is solved (functional organizational criteria work as well as formal ones, and clarity can be achieved without state-centricity). The disappearance_verdict is 'world_rearranges'—removal of the state-centric criterion would shift legal protections toward non-state groups, alter prosecution patterns, and reshape conflict incentives. The constraint's mandate (clarifying combatant status) has NOT outlived its function (the function is still needed), but the extraction component has accumulated and now dominates enforcement activity (rising theater_ratio). This is not classical mandatrophy (dead mandate), but it is approaching a secondary mandatrophy: the constraint persists partly because beneficiary states choose to maintain it despite its mandate being substantially addressed. The theater_ratio rising from 0.22 to 0.42 indicates that enforcement activity is increasingly theatrical—defending the formal criterion rather than serving clarity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_formal_criterion_contest,
    'Is the requirement for formal state organization a necessary clarity mechanism for distinguishing legitimate combatants, or is it a structural exclusion that prevents functionally equivalent non-state organizations from accessing protections they have earned through compliance and organization?',
    'Comparative analysis of non-state armed groups meeting functional criteria (command structure, unit discipline, humanitarian compliance, territorial control): do they operationally distinguish combatants from civilians as effectively as state militaries? Do they execute Geneva compliance measurably? Historical case studies (PKK, YPG, FARC ceasefire monitors, Palestinian security forces) document functional equivalence; the question is whether functional data overrides formal criteria.',
    'If functional equivalence is established, the state-centric reading loses its legitimacy claim and computes as pure institutional extraction. The constraint would reclassify toward snare. If formal clarity is demonstrated as necessary and unavoidable (functional groups reliably revert to targeting civilians without formal accountability structures), the reading''s tangled-rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_formal_criterion_contest, empirical, 'Whether formal state organization is a necessary clarity mechanism or an arbitrary institutional privilege.').

omega_variable(
    ap_i_article_1_4_interpretive_contest,
    'Does AP I Article 1(4), which extends combatant status to non-state actors fighting colonial, occupational, or racist regimes, meaningfully override the state-centric reading, or is it dead law in practical enforcement?',
    'Survey of ICJ decisions, state practice in detention and prosecution of AP I-eligible fighters, treaty body interpretations, and military manuals: is Article 1(4) applied by signatory states, or is it formally on the books while operationally ignored? Count prosecutions of AP I-eligible fighters as unlawful combatants versus grants of POW status; track state positions in treaty bodies.',
    'If AP I Article 1(4) is operationally dead despite formal ratification, the state-centric reading achieves hegemonic de facto status and its extraction is higher. If AP I is operationally live (states grant status, courts apply it), then the state-centric reading coexists with a functional alternative and its extractive power is moderated by judicial precedent. High operational usage = constraint is genuinely contested and extraction is lower; zero usage = constraint is unchallenged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ap_i_article_1_4_interpretive_contest, empirical, 'Whether AP I Article 1(4) is operationally enforced or formally dead.').

omega_variable(
    identity_lock_mechanism_for_fighters,
    'For non-state fighters classified as unlawful combatants, is the measured suppression structural (legal barriers, international enforcement preventing status access) or internalized (fighters have adopted state narratives about illegitimacy and accept denial of status as deserved)?',
    'Post-conflict reconciliation narratives, interviews with former fighters regarding perceived legitimacy of their combatant claim, truth commission testimony: do fighters emerging from status denial reclaim the legitimacy of their combat, or do many accept the unlawful label as accurate? High internalization = fighters believe they deserve denial; low internalization = structural suppression is carrying the constraint.',
    'If suppression is substantially internalized, non-state fighters are identity-locked into accepting unlawful status, and exit via legal status-claiming is psychologically unavailable; effective suppression is higher than the structural measure. If suppression is structural only, exit via legal change is possible if rules change, and the constraint is more vulnerable to revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_fighters, empirical, 'Whether suppression of non-state combatant status is structural or internalized.').

omega_variable(
    hegemonic_enforcement_mechanism_sustainability,
    'The state-centric reading depends on sustained enforcement by powerful states (prosecution of non-state fighters, treaty interpretation, diplomatic pressure). Is this enforcement sustainable, or are challenger states building alternative legal regimes (African Union protocols, BRICS legal frameworks) that will fragment the rule?',
    'Track development of non-Western legal instruments addressing combatant status, survey state positions in recent AP I review conferences, monitor use of alternative frameworks in regional conflicts (African Union, Arab League, Shanghai Cooperation Organization). Count states overtly rejecting hegemonic interpretation.',
    'If enforcement is fragmenting, the constraint''s extractive power declines because non-state actors in non-hegemonic-aligned regions can access alternative status frameworks. The state-centric reading becomes one reading among many rather than hegemonic. If enforcement is consolidating, extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hegemonic_enforcement_mechanism_sustainability, empirical, 'Whether hegemonic enforcement of the state-centric reading is sustainable or fragmenting.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Can the state-centric reading and the functional-protection reading coexist within a single coherent legal framework, or do they logically foreclose each other?',
    'Logical analysis of their foundational axioms: state-centric requires exclusion of non-state combatants; functional-protection requires inclusion of functionally equivalent actors. Can a framework that includes both simultaneously be coherent? Or must one be chosen?',
    'This determines whether the reading_relations should be ''forecloses'' (one must win) or ''coexists_with'' (both can be held by different parties, creating a live contest). If they foreclose, the constraint is on a trajectory toward binary victory or defeat. If they coexist, the constraint persists as a contested institutional division.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the state-centric reading logically forecloses functional protection readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t0, combatant_status_definition__state_centric_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(comb_tr_t0, observed).
narrative_ontology:measurement(comb_tr_t10, combatant_status_definition__state_centric_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(comb_tr_t10, observed).
narrative_ontology:measurement(comb_tr_t20, combatant_status_definition__state_centric_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(comb_tr_t20, observed).
narrative_ontology:measurement(comb_tr_t30, combatant_status_definition__state_centric_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(comb_tr_t30, observed).
narrative_ontology:measurement(comb_tr_t45, combatant_status_definition__state_centric_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement_basis(comb_tr_t45, observed).
narrative_ontology:measurement(comb_tr_t60, combatant_status_definition__state_centric_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(comb_tr_t60, observed).
narrative_ontology:measurement(comb_tr_t75, combatant_status_definition__state_centric_reading, theater_ratio, 75, 0.42).
narrative_ontology:measurement_basis(comb_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(comb_be_t0, combatant_status_definition__state_centric_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(comb_be_t0, observed).
narrative_ontology:measurement(comb_be_t10, combatant_status_definition__state_centric_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(comb_be_t10, observed).
narrative_ontology:measurement(comb_be_t20, combatant_status_definition__state_centric_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(comb_be_t20, observed).
narrative_ontology:measurement(comb_be_t30, combatant_status_definition__state_centric_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(comb_be_t30, observed).
narrative_ontology:measurement(comb_be_t45, combatant_status_definition__state_centric_reading, base_extractiveness, 45, 0.76).
narrative_ontology:measurement_basis(comb_be_t45, observed).
narrative_ontology:measurement(comb_be_t60, combatant_status_definition__state_centric_reading, base_extractiveness, 60, 0.79).
narrative_ontology:measurement_basis(comb_be_t60, observed).
narrative_ontology:measurement(comb_be_t75, combatant_status_definition__state_centric_reading, base_extractiveness, 75, 0.79).
narrative_ontology:measurement_basis(comb_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t0, combatant_status_definition__state_centric_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(comb_su_t0, observed).
narrative_ontology:measurement(comb_su_t10, combatant_status_definition__state_centric_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(comb_su_t10, observed).
narrative_ontology:measurement(comb_su_t20, combatant_status_definition__state_centric_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(comb_su_t20, observed).
narrative_ontology:measurement(comb_su_t30, combatant_status_definition__state_centric_reading, suppression_requirement, 30, 0.77).
narrative_ontology:measurement_basis(comb_su_t30, observed).
narrative_ontology:measurement(comb_su_t45, combatant_status_definition__state_centric_reading, suppression_requirement, 45, 0.8).
narrative_ontology:measurement_basis(comb_su_t45, observed).
narrative_ontology:measurement(comb_su_t60, combatant_status_definition__state_centric_reading, suppression_requirement, 60, 0.81).
narrative_ontology:measurement_basis(comb_su_t60, observed).
narrative_ontology:measurement(comb_su_t75, combatant_status_definition__state_centric_reading, suppression_requirement, 75, 0.81).
narrative_ontology:measurement_basis(comb_su_t75, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=75
narrative_ontology:measurement(comb_grid_01, combatant_status_definition__state_centric_reading, accessibility_collapse(class), 0, 0.75).
narrative_ontology:measurement(comb_grid_02, combatant_status_definition__state_centric_reading, accessibility_collapse(class), 75, 0.78).
narrative_ontology:measurement(comb_grid_03, combatant_status_definition__state_centric_reading, accessibility_collapse(individual), 0, 0.65).
narrative_ontology:measurement(comb_grid_04, combatant_status_definition__state_centric_reading, accessibility_collapse(individual), 75, 0.7).
narrative_ontology:measurement(comb_grid_05, combatant_status_definition__state_centric_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(comb_grid_06, combatant_status_definition__state_centric_reading, accessibility_collapse(organizational), 75, 0.76).
narrative_ontology:measurement(comb_grid_07, combatant_status_definition__state_centric_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(comb_grid_08, combatant_status_definition__state_centric_reading, accessibility_collapse(structural), 75, 0.73).
narrative_ontology:measurement(comb_grid_09, combatant_status_definition__state_centric_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(comb_grid_10, combatant_status_definition__state_centric_reading, resistance(class), 75, 0.7).
narrative_ontology:measurement(comb_grid_11, combatant_status_definition__state_centric_reading, resistance(individual), 0, 0.58).
narrative_ontology:measurement(comb_grid_12, combatant_status_definition__state_centric_reading, resistance(individual), 75, 0.6).
narrative_ontology:measurement(comb_grid_13, combatant_status_definition__state_centric_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(comb_grid_14, combatant_status_definition__state_centric_reading, resistance(organizational), 75, 0.68).
narrative_ontology:measurement(comb_grid_15, combatant_status_definition__state_centric_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(comb_grid_16, combatant_status_definition__state_centric_reading, resistance(structural), 75, 0.62).
narrative_ontology:measurement(comb_grid_17, combatant_status_definition__state_centric_reading, stakes_inflation(class), 0, 0.71).
narrative_ontology:measurement(comb_grid_18, combatant_status_definition__state_centric_reading, stakes_inflation(class), 75, 0.76).
narrative_ontology:measurement(comb_grid_19, combatant_status_definition__state_centric_reading, stakes_inflation(individual), 0, 0.78).
narrative_ontology:measurement(comb_grid_20, combatant_status_definition__state_centric_reading, stakes_inflation(individual), 75, 0.82).
narrative_ontology:measurement(comb_grid_21, combatant_status_definition__state_centric_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(comb_grid_22, combatant_status_definition__state_centric_reading, stakes_inflation(organizational), 75, 0.68).
narrative_ontology:measurement(comb_grid_23, combatant_status_definition__state_centric_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(comb_grid_24, combatant_status_definition__state_centric_reading, stakes_inflation(structural), 75, 0.64).
narrative_ontology:measurement(comb_grid_25, combatant_status_definition__state_centric_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(comb_grid_26, combatant_status_definition__state_centric_reading, suppression(class), 75, 0.75).
narrative_ontology:measurement(comb_grid_27, combatant_status_definition__state_centric_reading, suppression(individual), 0, 0.72).
narrative_ontology:measurement(comb_grid_28, combatant_status_definition__state_centric_reading, suppression(individual), 75, 0.78).
narrative_ontology:measurement(comb_grid_29, combatant_status_definition__state_centric_reading, suppression(organizational), 0, 0.6).
narrative_ontology:measurement(comb_grid_30, combatant_status_definition__state_centric_reading, suppression(organizational), 75, 0.68).
narrative_ontology:measurement(comb_grid_31, combatant_status_definition__state_centric_reading, suppression(structural), 0, 0.54).
narrative_ontology:measurement(comb_grid_32, combatant_status_definition__state_centric_reading, suppression(structural), 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__state_centric_reading, 0.18).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, international_humanitarian_law_compliance_regime).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, war_crimes_prosecution_framework).
narrative_ontology:affects_constraint(combatant_status_definition__state_centric_reading, ap_i_article_1_4_liberation_clause).

% DUAL FORMULATION NOTE:
% This constraint is one member of a three-reading family interpreting the 'combatant_status_definition' kernel. The functional_protection_reading (separate story) lowers ε by extending protections regardless of status; the national_liberation_reading (separate story) lowers ε by extending status to AP I-eligible groups. The state-centric reading has the highest ε because it is most restrictive. Network edges connect all three: each reading affects how the others operate in practice. The upstream constraint (state_centric) influences downstream readings because hegemonic states use the state-centric interpretation to block AP I 1(4) applicability in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__state_centric_reading, organized, 0.74).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
