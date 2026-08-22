% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Revolutionary Vanguard Jihad Obligation (Takfir-Emergency Reading)
 *   domain: religious/political-theological
 *
 * SUMMARY:
 *   The Quranic fighting-verses corpus supports multiple structurally
 *   distinct arrangements; this story instantiates one of them — the
 *   revolutionary vanguard reading, in which defensive jihad collapses into
 *   an immediate individual obligation (fard 'ayn) that every Muslim owes
 *   without state authorization, activated against rulers declared apostate
 *   (takfir) and foreign occupiers, with emergency jurisprudence suspending
 *   the classical safeguards (invitation, imam authority, proportionality)
 *   and collective-guilt doctrines reclassifying civilians as combatants. The
 *   standing arrangement under contest — the referent for every authored
 *   value — is the vanguard mobilization itself: the networks, fronts,
 *   enforcement machinery, and media operations running from Qutb's
 *   Milestones (1964) through the takfiri currents of the 1970s (The
 *   Neglected Duty), al-Qaeda's mass-casualty turn, the Islamic State's
 *   territorial caliphate, and the present post-territorial insurgency. The
 *   claimed type (snare) is authored from the structural read that the
 *   defensive coordination story is real but subordinated — the arrangement's
 *   distinctive machinery (takfir, emergency override, collective guilt)
 *   exists to concentrate interpretive authority and direct violence at the
 *   ummah's own members — while the metrics are authored independently from
 *   the arrangement's observed operation. The reading's endorsed alternative
 *   (restored divine governance) is NOT the referent; the referent is what
 *   the vanguard arrangement actually does. Any divergence between claim and
 *   computed per-seat types is the measurement the corpus exists to take. KEY
 *   AGENTS (by structural relationship): - vanguard_leadership: agenda-setter
 *   and principal beneficiary (organized/identity_locked) — claims takfir
 *   authority, directs fronts, captures authority and resources -
 *   takfir_cleric_network: beneficiary (moderate/identity_locked) — supplies
 *   religious authorization for the targeting doctrines - recruiter_networks:
 *   beneficiary (moderate/constrained) — moves personnel, funds, and
 *   propaganda toward the fronts - rank_and_file_recruits: principal target
 *   among participants (powerless/identity_locked) — bear the mortal costs,
 *   receive meaning and belonging -
 *   muslim_civilians_in_contested_territories: target (powerless/trapped) —
 *   bear collective-guilt liability and conscription -
 *   apostate_rulers_designated: target (institutional/trapped) — heads of
 *   states declared illegitimate - foreign_occupation_forces: target with
 *   mobility (institutional/mobile) — the genuine defensive object of the
 *   shared kernel - rival_mainstream_clerics: target and excluded voice
 *   (institutional/constrained) — declared apostates for rejecting takfir -
 *   diaspora_youth_recruitment_pool: prospective target (powerless/mobile) —
 *   the recruitment frontier - counterterrorism_analysts: analytical observer
 *   (institutional/analytical) — sees the whole structure, commands none of
 *   it
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Revolutionary Vanguard Jihad Obligation (Takfir-Emergency Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political-theological").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'fa45d037-536d-4afa-a793-8258ee7707fe').
narrative_ontology:cs_kernel_codification('fa45d037-536d-4afa-a793-8258ee7707fe', fixed_text).
narrative_ontology:cs_authority_grounding('fa45d037-536d-4afa-a793-8258ee7707fe', extraction).
narrative_ontology:cs_interpretation_layer_present('fa45d037-536d-4afa-a793-8258ee7707fe').
narrative_ontology:cs_reading_relation('fa45d037-536d-4afa-a793-8258ee7707fe', jihad_quranic_corpus__defensive_spiritual_reading, influences).
narrative_ontology:cs_reading_relation('fa45d037-536d-4afa-a793-8258ee7707fe', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('fa45d037-536d-4afa-a793-8258ee7707fe', foundational, takfir_authorizes_individual_war).
narrative_ontology:cs_axiom_status(takfir_authorizes_individual_war, holdable).
narrative_ontology:cs_axiom_grounding('fa45d037-536d-4afa-a793-8258ee7707fe', takfir_authorizes_individual_war, theological).
narrative_ontology:cs_axiom('fa45d037-536d-4afa-a793-8258ee7707fe', foundational, emergency_nullifies_jurisprudential_safeguards).
narrative_ontology:cs_axiom_status(emergency_nullifies_jurisprudential_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('fa45d037-536d-4afa-a793-8258ee7707fe', emergency_nullifies_jurisprudential_safeguards, instrumental).
narrative_ontology:cs_axiom('fa45d037-536d-4afa-a793-8258ee7707fe', secondary, collective_guilt_extends_liability_to_civilians).
narrative_ontology:cs_axiom_status(collective_guilt_extends_liability_to_civilians, holdable).
narrative_ontology:cs_axiom_grounding('fa45d037-536d-4afa-a793-8258ee7707fe', collective_guilt_extends_liability_to_civilians, theological).
narrative_ontology:cs_reference_frame('fa45d037-536d-4afa-a793-8258ee7707fe', prophetic_vanguard_community_norm).
narrative_ontology:cs_drift_state('fa45d037-536d-4afa-a793-8258ee7707fe', contemporary_umma_condition, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('fa45d037-536d-4afa-a793-8258ee7707fe', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_cleric_network).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, recruiter_networks).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, rank_and_file_recruits).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_in_contested_territories).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers_designated).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, foreign_occupation_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_mainstream_clerics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, rank_and_file_recruits).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, diaspora_youth_recruitment_pool).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, fard_ayn_emergency_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, divine_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims authority to declare rulers and rival clerics apostates and to summon individual Muslims to war without state authorization. Directs cells and fronts, controls funds and media output, and its standing depends entirely on the continuation of the mobilization it commands. Its members face capture or death if they abandon the project, and their authority dissolves if the takfir claim is withdrawn.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership, agenda_setter,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership, beneficiary).

% Jurists and ideologues who produce the rulings designating apostate rulers, legitimizing violence against rival Muslims, and reclassifying civilian categories. They hold no state office; their standing comes from the vanguard's need for religious authorization, and they gain audiences, donations, and status that mainstream institutions deny them. Leaving the network means repudiating their own rulings and exposing themselves to the same charges they issue.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, takfir_cleric_network, beneficiary,
    moderate, generational, identity_locked, global).

% Operate online propaganda, translation cells, and facilitation pipelines that move people and money toward the fronts. They collect status within the movement and material support from donors; their operational security depends on the flow of new recruits continuing. Capture by security services is their principal risk; exit into ordinary life means prosecution exposure.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, recruiter_networks, beneficiary,
    moderate, immediate, constrained, global).

% Fight, courier, build, and die for the fronts. They receive belonging, purpose, promised martyrdom, and in some territories wages and status; they bear the mortal risk, the severing of family ties, and in defeat the legal and social consequences. Leaving means apostasy in the eyes of their own cell, social death inside the network, and often execution; staying means the next operation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, rank_and_file_recruits, payer,
    powerless, immediate, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, rank_and_file_recruits, beneficiary).

% Young Muslims, often second-generation and socially marginal, exposed to vanguard media. They have not yet joined; the arrangement's future personnel and the leadership's claims both depend on moving some of them from grievance to enlistment. Their options — ordinary life, non-violent activism, emigration — remain open until they enlist, which is why the propaganda targets them before identity hardens.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, diaspora_youth_recruitment_pool, payer,
    powerless, immediate, mobile, global).

% Live under fronts that claim to defend them while conscripting their sons, taxing their trade, and punishing dissent as apostasy. Collective-guilt rulings classify those who work with governments or fail to support the fronts as legitimate targets. They cannot leave the war zones, and their protection claims have no seat in any vanguard council.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_in_contested_territories, payer,
    powerless, biographical, trapped, regional).

% Rule states the vanguard declares illegitimate for governing by man-made law and allying with foreign powers. They face insurgency, assassination, and delegitimation campaigns; their countermeasures — security services, counter-fatwas, repression — feed the vanguard's narrative. They cannot concede the vanguard's premise without dissolving their own legitimacy, and cannot exit their position without losing the state.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers_designated, payer,
    institutional, generational, trapped, national).

% Deployed military forces in Muslim-majority territories that the vanguard designates as invaders. They bear the direct military costs of the insurgency — casualties, deployment extensions, security spending — but unlike the other targets they retain the option of withdrawal, and their presence is the grievance that anchors the vanguard's defensive claim.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, foreign_occupation_forces, payer,
    institutional, biographical, mobile, regional).

% Hold positions in state-linked institutions such as al-Azhar and national councils, teach the classical conditions for armed jihad, and issue counter-fatwas against takfir. The vanguard declares them apostates for legislating alongside rulers, which legitimizes their assassination. They are outside the vanguard's deliberative frame entirely — their objections are pre-classified as the words of apostates — while their institutions remain exposed to its violence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_mainstream_clerics, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_mainstream_clerics, excluded).

% Government analysts and academic researchers who map the networks, track financing and propaganda, and assess which grievances are genuine versus manufactured. They see the whole structure across jurisdictions but command none of it; their products shape policy toward the fronts without altering the doctrine itself.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, counterterrorism_analysts, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_leadership).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: When state institutions are judged corrupt, absent, or complicit with occupation, the arrangement solves the mobilization problem of defense: it converts a collective good — protection of Muslim lands and governance by divine law — into an individual duty, so that defense no longer depends on a legitimate commander's call. It also coordinates dispersed individuals into cells and fronts without state infrastructure.
% TRANSFER_FUNCTION: Moves lives, labor, and money from individual Muslims — recruits, sympathizers, donors, and populations under front control — to the armed struggle and to those who command it; moves interpretive authority over war and apostasy from established jurists and states to vanguard clerics; and imposes the security costs of the struggle on civilians in contested territories.
% ABSENT_VOICES: Mainstream jurists and the Muslim publics who follow them are structurally absent: the takfir framework pre-classifies their objections as apostasy, so no seat exists in vanguard councils for the classical safeguards (invitation, imam authorization, proportionality) or for the civilians the collective-guilt rulings target. Their absence is what makes the vanguard's internal unanimity look like consensus.
% DISAPPEARANCE_RATIONALE: If the obligation and its enforcement machinery vanished overnight, the fronts would lose their doctrinal glue within a generation — cells would dissolve into ordinary politics, migration, or quietism, states would regain security bandwidth, and mainstream jurisprudence would reabsorb the defensive question. The underlying grievances (occupation, authoritarian misrule) would persist, but the specific structure — individual duty, takfir of rulers, emergency override — organizes real careers, funds, wars, and deaths that would not otherwise exist in this form.
% FOUNDING_PROBLEM: How can the Muslim community defend itself and live under divine sovereignty when its rulers are impotent, complicit with colonial powers, or have abandoned jihad — a problem posed in its modern form by the collapse of the caliphate (1924), colonial occupation, and the post-1967 crisis that Qutb's Milestones crystallized.
% FOUNDING_PROBLEM_CORROBORATION: The grievance substrate is corroborated from outside the benefiting parties: mainstream clerical bodies (al-Azhar, the Amman Message signatories), academic historians of the movement, and former members all attest that occupation and authoritarian misrule are real and that defensive war against invasion is classically legitimate. Those same sources, from outside the vanguard, attest that the distinctive vanguard solution — takfir of Muslim societies, individual duty bypassing all authority, emergency override of safeguards — addresses a problem the vanguard's own doctrine largely manufactured. No source outside the benefiting parties attests that the takfir expansion itself is required by the founding problem.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.85 at interval end) because the arrangement takes lives (recruits', and civilians' under collective-guilt rulings), autonomy (exit sealed by takfir), and interpretive authority (from established jurists and states to the vanguard), while the defensive service it nominally provides is real but partial. Suppression (0.85) is authored as a raw structural property — it is NOT scaled by power or scope; the engine scales only extractiveness — and reflects the takfir death-penalty logic applied to dissent, cell isolation designed to prevent coalition formation among victims, and the delegitimation of every alternative reading; the structural/internalized split is carried by a dedicated omega. Theater_ratio (0.55) reflects the growing share of activity that is media production, martyrdom mythology, and claimed operations whose function is recruitment and authority maintenance rather than territorial defense; it rises with the propaganda apparatus and peaks after territorial loss, when performance substitutes for governance. Accessibility_collapse (0.6) is moderate: mainstream jurisprudence, ordinary politics, and emigration remain real and widely chosen alternatives — the constraint's frame delegitimizes them without collapsing them for the population at large, though they collapse almost entirely for an enrolled recruit. Resistance (0.7) is high: states, mainstream clerical bodies, and Muslim publics fight the arrangement militarily and discursively (counter-fatwa campaigns, the Amman Message, counterterrorism operations). All three series run on one shared time grid (T=0,10,20,30,40,50,60) so the engine samples every metric at every authored point; the trajectory rises monotonically to the territorial-caliphate peak (T=50) and eases slightly after territorial loss (T=60) — a drift, not a cycle. Boltzmann note: identity_coordination is declared because takfir is boundary maintenance of who counts as Muslim, but this is precisely the gaming shape the framework warns about — the identity frame is the cover story, and the coupling concentrates costs on powerless agents (recruits, civilians) at global scope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the vanguard leadership's position the arrangement is divine obligation it administers: the costs it imposes are sacrifices, the authority it concentrates is stewardship, and the ummah's rejection is evidence of apostasy rather than of the arrangement's failure. From the recruit's seat the same structure is meaning fused with mortal risk and an exit that equals damnation; from the civilian's seat it is conscription, taxation, and liability to targeting; from the mainstream cleric's seat it is heresy weaponized against the jurisprudential tradition itself; from the occupation force's seat it is an insurgency it can leave by withdrawing, unlike every other target. The engine computes this divergence per seat from power, exit, and declared position; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map to directionality as follows. Vanguard_leadership, takfir_cleric_network, and recruiter_networks are declared beneficiaries: low d, and the engine damps or inverts their effective extraction into subsidy — they collect authority, standing, and funds from the arrangement they run or serve. Rank_and_file_recruits are declared victims with identity_locked exit: identity-locked targets sit near the full-target end, so their effective extraction is amplified beyond what powerlessness alone would produce. Muslim_civilians_in_contested_territories are trapped, powerless victims at regional scope: near-full targets with no exit damping. Apostate_rulers_designated are victims with institutional power at national scope — high d with scope amplification, but their institutional power moderates what the arrangement can take from them relative to civilians. Foreign_occupation_forces are victims with mobile exit: the withdrawal option pulls them toward the middle of the d-range relative to trapped victims — the structural signature of the genuine defensive component shared with the sibling readings. Rival_mainstream_clerics are targeted victims (takfir legitimizes their killing) with institutional standing and constrained exit. No directionality_overrides are used: the derivation from beneficiary/victim declarations plus power and exit atoms produces the right d at every seat, including the recruit/civilian asymmetry (identity lock) and the occupier/civilian asymmetry (mobility).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — defense of the community when its institutions fail — is real and partially live (occupations and authoritarian misrule persist), which is exactly what prevents mislabeling this arrangement as pure theater: there is concentrated benefit, active enforcement, and genuine defensive content. But the mandate has outrun its function: the takfir expansion converts the community the arrangement claims to defend into its principal target set, and the emergency doctrine manufactures the standing crisis that justifies the override. The arrangement persists through enforcement and identity fusion rather than through delivery of the founding good. This is why the claim is snare rather than tangled_rope: the coordination residue is real but subordinated to an authority project whose distinctive machinery exists to concentrate interpretive power. It is not a piton because maintenance is not theatrical inertia — it kills, recruits, and adapts, and its beneficiaries capture real rents. The single load-bearing point of the whole structure is the takfir premise: if the authority to declare Muslims apostate were withdrawn, the arrangement collapses into the defensive_spiritual_reading's much smaller constraint, which is why counter-mobilization that targets the takfir doctrine attacks the constraint's keystone rather than its periphery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the revolutionary_vanguard_reading of the jihad_quranic_corpus kernel; which structural elements would change under the sibling readings (defensive_spiritual_reading, expansionist_legalist_reading), and where exactly is the disagreement located?',
    'Compare the compiled sibling stories on the three contested elements: war authority (state/imam authorization vs. individual judgment via takfir), target class (invading combatants only vs. apostate Muslims and civilians via collective guilt), and safeguard status (binding vs. emergency-suspended). The disagreement is located in exactly these three elements; the defensive trigger and the underlying texts are shared.',
    'Under the defensive_spiritual_reading the victim set shrinks to invading combatants and the measured cost structure drops toward coordination cost; under the expansionist_legalist_reading authority returns to the imam and safeguards bind, restoring an authority-conditioned hybrid shape. This reading''s distinctive cost profile lives in the takfir and emergency-override elements; removing either collapses it toward a sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: one reading of a contested kernel; sibling readings alter victim set, war authority, and safeguard status.').

omega_variable(
    grievance_substrate_authenticity,
    'Is the defensive coordination function driven by genuinely experienced grievances (real occupation, real authoritarian predation) or manufactured and amplified by vanguard media and doctrine?',
    'Compare recruitment rates and front viability across territories with and without actual foreign occupation; grievance measurement in Muslim publics independent of vanguard media exposure; defection interviews distinguishing grievance-driven from identity-driven enlistment.',
    'If the grievance substrate is genuine, part of the measured cost structure is the price of real defense — a coordination residue inside the arrangement that the snare claim must be read against; if manufactured, the arrangement is extraction with a borrowed defensive warrant and the snare classification tightens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grievance_substrate_authenticity, empirical, 'Whether the defensive warrant is authentic grievance or manufactured mobilization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dissent and exit structural (takfir death-penalty logic, cell isolation, territorial control) or internalized (identity fusion in which leaving equals damnation and self-annihilation)?',
    'Post-exit trajectory of defectors: if fear of enforcement dominates defector testimony, suppression is structural; if defectors report continuing belief in the obligation after safe exit, suppression is substantially internalized. Compare defectors extracted by force against those who left through ideological disillusionment.',
    'If internalized, effective suppression exceeds the structural measure — recruits carry the constraint with them after physical exit, and counter-mobilization must target identity rather than enforcement capacity alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in recruit exit and dissent.').

omega_variable(
    civilian_liability_scope,
    'How far does the collective-guilt doctrine actually extend targeting to civilians in practice, as opposed to doctrine — and does practice track doctrine or pragmatic restraint?',
    'Casualty-pattern analysis across fronts and eras; internal documents distinguishing doctrinal claims from operational targeting rules; testimony of former commanders on how collective-guilt rulings were applied.',
    'If practice follows doctrine, civilian liability is a core operating feature and cost measures based on observed restraint understate it; if practice is restrained despite doctrine, the doctrine functions partly as performance and the theater_ratio is understated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_liability_scope, empirical, 'Doctrine-versus-practice gap in civilian targeting under collective guilt.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_vanguard_reading_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t0, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t10, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t20, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t30, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t40, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t50, observed).
narrative_ontology:measurement(jihad_vanguard_reading_tr_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement_basis(jihad_vanguard_reading_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(jihad_vanguard_reading_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t0, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t10, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t20, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t30, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t40, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 50, 0.88).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t50, observed).
narrative_ontology:measurement(jihad_vanguard_reading_be_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement_basis(jihad_vanguard_reading_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(jihad_vanguard_reading_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t0, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t10, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t20, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t30, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t40, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t50, observed).
narrative_ontology:measurement(jihad_vanguard_reading_su_t60, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 60, 0.85).
narrative_ontology:measurement_basis(jihad_vanguard_reading_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% The Quranic jihad corpus is a single kernel decomposed into three constraint stories per the epsilon-invariance principle: the defensive_spiritual_reading (low extraction, combatant-only targets, authority-bound), the expansionist_legalist_reading (moderate extraction, imam-conditioned offensive jihad, safeguards binding), and this revolutionary_vanguard_reading (high extraction, takfir-expanded victim set, authority bypassed, safeguards suspended). The defensive reading is upstream: the vanguard reading parasitizes its defensive trigger and its texts to authorize targets and methods the defensive reading excludes. The legalist reading is the intermediate whose authority conditions the vanguard reading declares unsatisfiable in the current age. Each story carries its own epsilon, victim set, and classification; the family link routes contamination analysis — degradation of the defensive reading's boundary-keeping (who may legitimately be fought) propagates directly into this story's victim expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
