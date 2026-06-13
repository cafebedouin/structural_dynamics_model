% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Territorial Sovereignty as Existential Zero-Sum (Existential Matrix Reading)
 *   domain: political/international
 *
 * SUMMARY:
 *   This is the existential-matrix reading of the
 *   territorial-sovereignty-legitimacy kernel. It asserts that legitimacy
 *   derives not from legal/historical arguments (Balfour Declaration, UN
 *   Partition, ancient covenant, demographic majority) but from existential
 *   necessity: each people requires uncompromised territorial control as the
 *   only path to collective survival and identity preservation in a
 *   fundamentally zero-sum competition. Within this frame, compromise
 *   frameworks (two-state solution) are structurally unstable because neither
 *   side can accept the vulnerability inherent in territorial division—the
 *   other side's existence remains an existential threat. The conflict
 *   persists and intensifies not because legal settlements fail to address
 *   root causes, but because the existential frame makes compromise
 *   impossible: accepting less territory is experienced as accepting
 *   existential defeat. The beneficiary within this frame is whichever side
 *   achieves demographic/military dominance and can impose its will on the
 *   other. All other parties (civilians, diaspora populations, mediators) are
 *   trapped in the constraint's logic and bear its costs.
 *
 * KEY AGENTS:
 *   - palestinian_political_authority: Pursues territorial sovereignty under conditions of fragmentation and military imbalance; every compromise erodes existential security (territorial contiguity, demographic majority, buffer zones)
 *   - israeli_political_authority: Maintains dominance through military superiority and settlement expansion justified as existential necessity; any territorial concession reduces security buffer and risks demographic dilution
 *   - settler_movement: Drives territorial expansion as existential imperative; settlement expansion is experienced as civilizational necessity, not as contested policy
 *   - palestinian_diaspora: Identity fused with territorial claim; accepting permanent diaspora is experienced as civilizational erasure
 *   - jewish_diaspora: Israeli state existence is existentially significant as refuge post-genocide; any threat to Israel is civilizational threat
 *   - regional_rival_powers: Use the conflict as proxy theater; each power frames the constraint as existential (Sunni-Shia rivalry, revolutionary ideology, Arab nationalism); external actors block bilateral resolution
 *   - civilian_populations: Embedded in the existential frame through prolonged exposure; fear and existential anxiety become internalized; alternatives become unthinkable
 *   - peace_process_mediators: Propose compromise (two-state, confederation) but are structurally barred from success—mediators' proposals are read as naïve or favoring the other side
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.89).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.91).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty as Existential Zero-Sum (Existential Matrix Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, 'b1531235-e424-4697-b2d8-387c6489d048').
narrative_ontology:cs_kernel_codification('b1531235-e424-4697-b2d8-387c6489d048', distributed).
narrative_ontology:cs_authority_grounding('b1531235-e424-4697-b2d8-387c6489d048', extraction).
narrative_ontology:cs_reading_relation('b1531235-e424-4697-b2d8-387c6489d048', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('b1531235-e424-4697-b2d8-387c6489d048', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_axiom('b1531235-e424-4697-b2d8-387c6489d048', foundational, territorial_control_necessary_condition_survival).
narrative_ontology:cs_axiom_status(territorial_control_necessary_condition_survival, holdable).
narrative_ontology:cs_axiom_grounding('b1531235-e424-4697-b2d8-387c6489d048', territorial_control_necessary_condition_survival, empirically_contingent).
narrative_ontology:cs_axiom('b1531235-e424-4697-b2d8-387c6489d048', foundational, competing_survival_claims_zero_sum).
narrative_ontology:cs_axiom_status(competing_survival_claims_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('b1531235-e424-4697-b2d8-387c6489d048', competing_survival_claims_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('b1531235-e424-4697-b2d8-387c6489d048', mutual_existential_vulnerability_framework).
narrative_ontology:cs_drift_state('b1531235-e424-4697-b2d8-387c6489d048', contemporary_demographic_military_dominance, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b1531235-e424-4697-b2d8-387c6489d048', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinians).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_settlers).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_populations).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_diaspora).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, jewish_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_political_authority).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_political_authority).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, settler_movement).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, jewish_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_rival_powers).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, existential_security_primacy).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_control_necessity).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, demographic_dominance_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks territorial sovereignty and self-determination but operates under conditions of territorial fragmentation, military occupation, and demographic/military imbalance. Accepts settlements and agreements framed as compromise (two-state solution) but within this reading, every compromise erodes existential security—fewer contiguous territories, reduced demographic majority in residual state, vulnerability to blockade and siege. The exit from the framework is extinction or perpetual subordination.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_political_authority, payer,
    moderate, generational, trapped, regional).

% Maintains state sovereignty through military dominance and territorial control, justified as existential necessity against perceived annihilation threat. Within this reading, any territorial concession reduces security buffer, increases vulnerability to rocket fire and infiltration, and risks demographic dilution of Jewish majority. Two-state frameworks are experienced as structurally unstable—the other state's existence remains a permanent threat to survival.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_political_authority, payer,
    institutional, generational, constrained, regional).

% Drives territorial expansion as existential imperative: expanding the Jewish demographic footprint preempts Palestinian statehood, secures strategic depth, and prevents reversal of the 1948 territorial outcome. Frames settlement as fulfilling religious/national covenant and preventing existential threat. Exit from this activity means accepting vulnerability they perceive as civilizational suicide.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, settler_movement, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, settler_movement, payer).

% Depends on Palestinian state existence for collective identity and potential return/repatriation. Trapped in existential identity fusion: Palestinian identity is constituted by the territorial claim; accepting permanent diaspora status is experienced as civilizational erasure. Pays through remittances, advocacy burden, and intergenerational trauma while bearing no direct decision authority.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_diaspora, beneficiary,
    powerless, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_diaspora, payer).

% Israeli state existence is existentially significant as refuge and civilizational anchor after genocide. Provides material and diplomatic support; trapped in existential identification with Israeli security. Any existential threat to Israel is experienced as civilizational threat. Carries the constraint through international advocacy and strategic partnership.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, jewish_diaspora, beneficiary,
    powerful, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, jewish_diaspora, payer).

% Iran, Saudi Arabia, Egypt, Syria, Jordan use the conflict as proxy theater and legitimacy source. Each regional power frames the constraint through existential terms (Sunni-Shia rivalry, Arab nationalism, revolutionary ideological threat). Their involvement compounds the zero-sum dynamic and makes bilateral compromise impossible—external actors block resolution paths that would shift regional power balance.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_rival_powers, payer,
    institutional, generational, constrained, regional).

% UN, international courts, human-rights bodies document violations and issue rulings, but within this reading are structurally irrelevant—legal frameworks presume compromise and shared legitimacy sources (treaties, conventions, sovereignty equality). When legitimacy is existential rather than juridical, legal remedies address symptoms while the core dynamic (territorial zero-sum, demographic competition, military dominance) remains untouched. Observer position is analytically external; no power to alter the constraint.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_framework, observer,
    institutional, generational, analytical, global).

% Bear the daily costs of the constraint: military checkpoints, curfews, curfew-hours-to-school education, fear of violence, loss of livelihood and family members. Embedded in the existential framework through prolonged exposure—the frame becomes internalized; alternatives feel unthinkable. Exit is geographic displacement or death.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations, payer,
    powerless, biographical, trapped, local).

% US, EU, UN envoys propose compromise frameworks (two-state, confederation, shared security) but are structurally barred from success within this reading's logic: compromise presumes shared legitimacy sources (international law, mutual recognition, territorial trade-offs). If legitimacy is existential and zero-sum, compromise is experienced by both sides as capitulation, not settlement. Mediators' proposals are read as naïve or as advancing a hidden agenda favoring the other side.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_process_mediators, excluded,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading explicitly denies a coordination function; the constraint is pure conflict structure. The existential framework forecloses coordination: two peoples cannot simultaneously occupy the same territory with full demographic/political control. The 'solution' the constraint appears to solve (population coexistence) is structurally unsolvable within existential premises.
% TRANSFER_FUNCTION: Moves security and territorial control from one collective actor to another through military dominance and demographic expansion. Transfers also include: international recognition/legitimacy (who the UN seats), military aid and strategic partnership (US, Iran, Russia as patrons), reparations/refugee claims, and psychological/civilizational dominance (who is perceived as existentially threatened vs. threatening).
% ABSENT_VOICES: Israeli peace activists and Palestinian nonviolence movements are structurally marginalized within this reading because they deny the existential-zero-sum premise. If they were heard as authoritative, the reading would be falsified—their visibility is suppressed by both institutional and ideological gatekeeping. International mediators are also excluded from decision authority; their proposals are heard as irrelevant to existential stakes.
% DISAPPEARANCE_RATIONALE: If the existential frame vanished overnight—if both populations could genuinely accept territorial coexistence without existential anxiety—the entire conflict structure would decompose. Borders would become negotiable, security arrangements could be mutual rather than dominance-based, refugee/return claims could be traded off, and two functioning states with normalized relations would emerge. The constraint's disappearance is equivalent to the frame's dissolution.
% FOUNDING_PROBLEM: Territorial control is the precondition for collective survival: a people without territory cannot defend itself, preserve its culture, or prevent annihilation. Jewish survival after diaspora required territorial refuge; Palestinian survival requires prevention of demographic erasure. The founding problem is: how does a people achieve territorial security when another people claims the same land as existentially necessary?
% FOUNDING_PROBLEM_CORROBORATION: Both institutional actors (Israeli and Palestinian leaderships) attest the problem is live—security anxiety, threat perception, and existential concern are stated in their official rhetoric and strategic documents. Regional powers (Iran, Saudi Arabia, Syria) attest the problem is live through sustained proxy investment. Independent observers (human-rights organizations, conflict scholars, security analysts outside the benefiting parties) attest that the lived experience of civilians embeds existential fear on both sides, even when they dispute the origins or justice of that fear. No major institutional voice contests that existential security concerns drive the parties' negotiating positions.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.89 at present) and rising because the constraint continuously transfers security, territory, and demographic dominance from the weaker party (Palestinians) to the stronger party (Israel), with no stabilization point. The trend is monotonic: each conflict cycle produces territorial loss, demographic displacement, and reduced Palestinian capacity to maintain statehood. Suppression is higher still (0.91) because the constraint's persistence depends on active enforcement: military occupation, settlement expansion, legal restrictions on Palestinian movement and building, and suppression of nonviolent resistance. The enforcement is not incidental to the constraint but constitutive—without active suppression, the demographic balance would shift and the Israeli territorial claim would become untenable. Theater ratio (0.62) is moderate-high because a significant share of enforcement activity is presentational: the 'security' justification for checkpoints, curfews, and settlement expansion conceals the underlying driver (territorial dominance and demographic control). As the constraint matures, the security theater becomes more elaborate and more divorced from actual threat reduction. Accessibility collapse (0.93) is extremely high because alternatives to the existential frame are structurally unavailable once the frame is internalized: both populations have experienced decades of violence, displacement, and loss that confirm the frame's truth. Believing in peaceful coexistence requires denying lived experience—the frame becomes self-fulfilling. Resistance (0.87) is high because both populations actively resist: Palestinians through organized and individual resistance; Israelis through political resistance to concessions perceived as existential threat. The high resistance reflects that the constraint runs against genuine preferences on both sides—neither population benefits from the status quo, but both perceive the alternative as worse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Israeli political authority) and the payer (Palestinian political authority) should compute dramatically differently. From the Israeli institutional seat, the constraint is perceived as a security mechanism—extractiveness and suppression are justified costs of survival, theater ratio is reframed as necessary ambiguity (security operations cannot be fully transparent), and accessibility collapse reflects the objective absence of alternatives (integration with a hostile Arab-majority state would be suicidal). From the Palestinian seat, the same metrics describe occupation and ethnic cleansing—extractiveness is robbery, suppression is oppression, theater ratio is deception covering territorial theft, and accessibility collapse is structural entrapment with no exit except submission or erasure. The engine computes each seat's directionality from the structural data (Palestinian as high-d target, Israeli as low-d beneficiary), and the per-seat type divergence is the measurement the corpus exists to take: institutional seat perceives coordinated security, payer seat perceives systematic extraction. The authored claim (snare) captures the structural asymmetry; the per-seat computation should show conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian political authority sits at d ≈ 1.0 (full target): bears the extraction (territorial loss, displacement, sovereignty constraint), has trapped exit options (cannot leave the territory, cannot integrate with Israel, cannot achieve autonomy unilaterally), and is constrained by Israeli military dominance and international recognition of Israeli sovereignty. Israeli political authority sits at d ≈ 0.2 (approaching beneficiary): collects the extraction (territorial control, demographic dominance, international recognition, security buffer), has constrained exit (cannot withdraw without accepting existential anxiety), and is protected by military superiority and diplomatic weight. Settler movement sits at d ≈ 0.15 (strong beneficiary): directly benefits from territorial expansion, has identity-locked exit (cannot exit without existential identity loss), and drives the constraint's trajectory. Civilian populations on both sides sit at d ≈ 0.95 (near-complete targets): bear daily costs, have trapped exit, and are embedded in the existential frame through prolonged exposure. The divergence reflects that the constraint's primary function is not coordination but asymmetric extraction under the guise of security necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (collective survival requiring territorial control) was live in 1948 and remains live in 2026—neither population has achieved secure territorial dominance, and both continue to perceive existential threats. However, mandatrophy detection hinges on the (founding_problem_status × disappearance_verdict) mismatch: the founding problem is LIVE (correct perception: territorial security is still contested), and the verdict is WORLD_REARRANGES (correct: the constraint's disappearance would fundamentally restructure the political landscape). These align rather than mismatch, so no mandatrophy flag fires. However, the commentary on founding_problem_corroboration reveals the diagnostic tension: both parties attest the founding problem is live, but independent observers (security analysts, peace researchers) increasingly attest that the founding problem has been SOLVED at the material level (Israel has achieved overwhelming military dominance, demographic Jewish majority, and international recognition; Palestinians have accepted a truncated state and limited sovereignty), and the constraint persists not because survival is threatened but because the existential frame is now self-perpetuating and the institutional actors benefit from the constraint's continuance. This suggests the founding problem has DIED (the objective threat has been neutralized) while the constraint persists, which is a mandatrophy signature: zombie constraint carrying a dissolved founding problem. The narrative corroboration pattern (benefiting parties claim liveness, independent observers claim death) is the classic false-summit sign under a different reading—the constraint masquerades as existential necessity but functions as institutional rent-collection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_constructed_frame,
    'Is the existential frame a description of genuine survival conditions, or is it a constructed narrative that exaggerates threat and forecloses compromise?',
    'Empirical analysis: Compare actual military/demographic threat metrics against stated existential claims. Track whether existential anxiety correlates with actual threat conditions or with political rhetoric cycles. Examine cases where existential frame has been abandoned (e.g., Cold War NATO-Soviet mutual accommodation) to identify conditions for frame dissolution.',
    'If the frame is constructed rather than emergent from material conditions, the constraint is a Snare maintained through narrative control and could be undone by reframing. If the frame reflects genuine survival vulnerabilities, the constraint is partially emergent from structural conditions and would require material security guarantees rather than rhetorical reframing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_constructed_frame, empirical, 'Whether existential security concerns are description or construction.').

omega_variable(
    alternative_security_architectures,
    'Are mutual security guarantees (external security commitments, demilitarization treaties, alliance structures) structurally capable of substituting for unilateral territorial control?',
    'Historical comparison: examine Cold War deterrence, NATO expansion, or post-WWII occupation transitions where existential anxiety was managed through guarantees rather than territorial expansion. Identify the guarantor reliability threshold—at what level of credibility do parties accept shared security frameworks?',
    'If credible mutual security architecture is possible, the zero-sum territorial competition could be decoupled from existential anxiety, and compromise frameworks (two-state with security guarantees) would become viable. If guarantor credibility is structurally impossible (as this reading suggests), the existential frame is robust and territorial expansion is the only perceived path to security.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_architectures, empirical, 'Whether existential anxiety can be managed through non-territorial means.').

omega_variable(
    demographic_vs_political_majority,
    'Does existential legitimacy require demographic majority, or can it rest on political/military control alone?',
    'Examine settled conflicts where minority rule persists (e.g., Singapore, Northern Ireland, post-genocide Rwanda) to identify stability conditions. Track whether demographic anxiety drives escalation in this conflict more than political control questions.',
    'If demographic majority is structurally necessary for existential security, territorial expansion and settlement policy are rational responses within the frame, and compromise (accepting Palestinian majority in residual territory) is experienced as capitulation. If political control suffices, the frame could accept minority status within a federal arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_vs_political_majority, empirical, 'Whether existential security requires demographic dominance or political control.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the existential-matrix reading logically foreclose the self-determination and covenant-continuity readings within a single coherent framework, or can all three readings coexist as different parties'' competing legitimacy claims?',
    'Examine whether an actor could simultaneously hold: (a) existential legitimacy (this reading), (b) self-determination rights (self-determination reading), and (c) covenant legitimacy (covenant-continuity reading) without internal contradiction. If yes, the readings coexist. If holding one reading''s core premise requires denying the others'' premises, foreclosure applies.',
    'If foreclosure obtains, this reading is ontologically incompatible with the siblings—only one can be true. If coexistence obtains, the kernel is genuinely contested across parties, and the conflict is a contest between incommensurable legitimacy frameworks rather than a factual dispute. The foreclosure question determines whether resolution requires converting parties to a shared framework (very hard) or managing incommensurable claims within a structural accommodation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether this reading''s core premise is logically compatible with sibling readings'' core premises.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.91) structural (external barriers: military force, legal prohibition, geographic isolation) or internalized (the parties believe they deserve the treatment, have fused their identity with the conflict, or have limited contact with alternatives)?',
    'Post-ceasefire trajectory analysis: if suppression persists after external barriers are removed (as in Northern Ireland post-1998), it is partially internalized. Track psychological/identity measures (anxiety, threat perception, willingness to engage with the other side) before and after conflict de-escalation.',
    'If suppression is primarily structural, removing the external constraints (military dominance, blockade, occupation) would reduce the constraint''s force. If suppression is internalized, the constraint would persist even after structural barriers fall—the parties would recreate them because the existential frame is now embedded in identity and psychology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    kernel_reading_disambiguation,
    'Is this reading (''territorial sovereignty as existential necessity'') actually instantiating a distinct constraint from the covenant-continuity and self-determination readings, or is it merely a different causal explanation for the same legal/political structure?',
    'Measure ε under each reading: if existential-matrix yields ε=0.89 (high extraction), covenant-continuity yields ε=0.55 (moderate), and self-determination yields ε=0.62 (moderate-high), the readings are distinct constraints (different ε values). If all three yield similar ε under their respective framing, decomposition is redundant and the kernel is a single constraint with multiple readings, not multiple constraints.',
    'Distinct-constraint status confirms this is one reading of a decomposed kernel family (per DP-001 ε-invariance principle). Same-ε status would indicate the reading is observational variation on one constraint, not a structural decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, empirical, 'Whether this reading generates a distinct ε value from sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.42).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1987, 0.51).
narrative_ontology:measurement_basis(terr_tr_t1987, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.56).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2010, 0.59).
narrative_ontology:measurement_basis(terr_tr_t2010, observed).
narrative_ontology:measurement(terr_tr_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2026, 0.62).
narrative_ontology:measurement_basis(terr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.72).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.79).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1987, 0.83).
narrative_ontology:measurement_basis(terr_be_t1987, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2010, 0.86).
narrative_ontology:measurement_basis(terr_be_t2010, observed).
narrative_ontology:measurement(terr_be_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2026, 0.89).
narrative_ontology:measurement_basis(terr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.68).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1987, 0.82).
narrative_ontology:measurement_basis(terr_su_t1987, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.87).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement_basis(terr_su_t2010, observed).
narrative_ontology:measurement(terr_su_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2026, 0.91).
narrative_ontology:measurement_basis(terr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.22).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel is decomposed into three distinct constraints corresponding to three readings of legitimacy grounds: (1) covenant_continuity_reading — legitimacy from ancient divine covenant, continuous presence, and modern recognition (ε ≈ 0.55, Mountain with false-summit candidates); (2) self_determination_reading — legitimacy from modern self-determination principle and demographic majority (ε ≈ 0.62, Tangled Rope with moderate extraction); (3) existential_matrix_reading (this story) — legitimacy from existential necessity for territorial control, zero-sum conflict (ε ≈ 0.89, Snare with high extraction). The three readings generate different ε values because they instantiate different constraints: they measure different things (historical legitimacy vs. demographic legitimacy vs. existential necessity), they propose different beneficiaries (Jewish people vs. Arab population vs. dominant demographic/military force), and they project different futures (covenant perpetuation vs. two-state solution vs. demographic/military resolution). The readings are held simultaneously by different parties and constitute the kernel's fundamental contestation. Each reading is a clean, ε-invariant constraint that stands alone; the family relationship is structural influence: the existential reading INFLUENCES the other two by creating existential pressure that destabilizes compromise frameworks the other readings propose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
