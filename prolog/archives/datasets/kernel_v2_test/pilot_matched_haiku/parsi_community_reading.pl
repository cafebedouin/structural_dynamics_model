% ============================================================================
% CONSTRAINT STORY: parsi_community_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parsi_community_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: parsi_community_reading
 *   human_readable: Parsi Community Marriage Authority (Zoroastrian Codified Reading)
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The Parsi community's marriage authority, codified in the Parsi Marriage
 *   and Divorce Act of 1936, represents one reading of a contested kernel:
 *   the question of who holds legitimate authority over marriage recognition,
 *   inheritance, and kinship in a pluralistic constitutional state. This
 *   reading instantiates Zoroastrian community self-governance grounded in
 *   religious tradition and codified by colonial-era statute. The constraint
 *   exhibits tangled_rope structure: it coordinates marriage recognition
 *   within the community (genuine coordination function) while extracting
 *   through endogamy enforcement and status asymmetry for non-Parsi spouses
 *   (asymmetric extraction). The constraint's persistence reflects both
 *   institutional inertia (the 1936 Act remains formally valid) and genuine
 *   community commitment to Zoroastrian cultural continuity. However, the
 *   constraint is increasingly pressured by constitutional rights frameworks
 *   and secular marriage alternatives, creating a drift toward lower
 *   suppression and stable extractiveness. The Parsi reading coexists with
 *   four sibling readings (Hindu codified, Muslim shariat, Christian
 *   colonial, secular contractual), each instantiating different authority
 *   structures and producing different beneficiary/victim distributions.
 *
 * KEY AGENTS:
 *   - Parsi Community Institutional Authority: Primary beneficiary (institutional/arbitrage) — maintains authority over marriage recognition, cultural continuity, endogamy enforcement
 *   - Parsi Individual Choosing Exogamy: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused with community; exit requires abandoning identity
 *   - Non-Parsi Spouse: Secondary victim (moderate/constrained) — faces status loss, religious identity contestation, exclusion from ceremonies
 *   - Indian Constitutional State: Organized actor (organized/constrained) — coordinates community autonomy while constraining through fundamental rights; constrained exit but significant agency
 *   - Colonial-Era Legal Framework: Institutional artifact (institutional/arbitrage) — persists through inertia; largely performative; maintains authority through formality rather than function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parsi_community_reading, 0.35).
domain_priors:suppression_score(parsi_community_reading, 0.42).
domain_priors:theater_ratio(parsi_community_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parsi_community_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(parsi_community_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(parsi_community_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parsi_community_reading, tangled_rope).
narrative_ontology:human_readable(parsi_community_reading, "Parsi Community Marriage Authority (Zoroastrian Codified Reading)").
narrative_ontology:topic_domain(parsi_community_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(parsi_community_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(parsi_community_reading, 'bdf4385a-1632-4120-b5d1-be0d3b21490a').
narrative_ontology:cs_kernel_codification('bdf4385a-1632-4120-b5d1-be0d3b21490a', formalized).
narrative_ontology:cs_authority_grounding('bdf4385a-1632-4120-b5d1-be0d3b21490a', lineage).
narrative_ontology:cs_interpretation_layer_present('bdf4385a-1632-4120-b5d1-be0d3b21490a').
narrative_ontology:cs_reading_relation('bdf4385a-1632-4120-b5d1-be0d3b21490a', parsi_community_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdf4385a-1632-4120-b5d1-be0d3b21490a', parsi_community_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdf4385a-1632-4120-b5d1-be0d3b21490a', parsi_community_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdf4385a-1632-4120-b5d1-be0d3b21490a', parsi_community_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('bdf4385a-1632-4120-b5d1-be0d3b21490a', foundational, zoroastrian_endogamy_essential_to_identity).
narrative_ontology:cs_axiom_status(zoroastrian_endogamy_essential_to_identity, holdable).
narrative_ontology:cs_axiom_grounding('bdf4385a-1632-4120-b5d1-be0d3b21490a', zoroastrian_endogamy_essential_to_identity, deontological).
narrative_ontology:cs_axiom('bdf4385a-1632-4120-b5d1-be0d3b21490a', foundational, community_self_governance_legitimate_authority).
narrative_ontology:cs_axiom_status(community_self_governance_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('bdf4385a-1632-4120-b5d1-be0d3b21490a', community_self_governance_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('bdf4385a-1632-4120-b5d1-be0d3b21490a', zoroastrian_community_self_governance).
narrative_ontology:cs_drift_state('bdf4385a-1632-4120-b5d1-be0d3b21490a', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bdf4385a-1632-4120-b5d1-be0d3b21490a', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(parsi_community_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parsi_community_reading, parsi_community_institutional_authority).
narrative_ontology:constraint_beneficiary(parsi_community_reading, endogamous_marriage_norm).
narrative_ontology:constraint_victim(parsi_community_reading, non_parsi_spouses).
narrative_ontology:constraint_victim(parsi_community_reading, parsi_individuals_choosing_exogamy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(parsi_community_reading, indian_constitutional_state).
narrative_ontology:constraint_victim(parsi_community_reading, parsi_individual_choosing_exogamy).
narrative_ontology:constraint_victim(parsi_community_reading, non_parsi_spouse).
narrative_ontology:constraint_vindicates(parsi_community_reading, religious_community_self_governance).
narrative_ontology:constraint_vindicates(parsi_community_reading, zoroastrian_cultural_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains authority over marriage recognition, solemnization, and religious identity transmission through the Parsi Marriage and Divorce Act. Sets rules for endogamy, inheritance, and kinship. Can arbitrage between community law and secular law; can modify rules through community consensus. Benefits from institutional authority and cultural continuity.
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_community_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% Seeks to marry outside the Parsi community. Structurally mobile (can legally marry non-Parsi partner) but identity-locked through internalized community belonging, family pressure, and fear of status loss. Exit from the constraint (marrying non-Parsi) requires abandoning identity as a full community member, losing access to religious ceremonies, and rupturing family relationships. Bears the cost of identity rupture or romantic autonomy sacrifice.
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_individual_choosing_exogamy, payer,
    powerless, biographical, identity_locked, regional).

% Marries a Parsi partner. Faces status loss within the community, exclusion from religious ceremonies, contested religious identity of children, and potential inheritance disputes. Can exit through divorce but faces relational costs. Experiences the constraint as asymmetric extraction through status loss and religious identity contestation.
narrative_ontology:constraint_stakeholder(parsi_community_reading, non_parsi_spouse, payer,
    moderate, biographical, constrained, regional).

% Recognizes Parsi personal law through constitutional provisions (Articles 25, 26) while constraining it through fundamental rights (Articles 14, 15, 21). Coordinates community autonomy while maintaining authority to invalidate discriminatory provisions. Benefits from community self-administration (reduces state burden) while bearing costs of adjudicating conflicts. Constrained exit (cannot fully withdraw recognition without constitutional crisis).
narrative_ontology:constraint_stakeholder(parsi_community_reading, indian_constitutional_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(parsi_community_reading, indian_constitutional_state, beneficiary).

% Colonial-era codification that froze Zoroastrian marriage practice into statute. Persists through institutional inertia and formal validity. Maintains authority through formality rather than functional force. Increasingly performative as secular alternatives become available and constitutional rights frameworks constrain its scope. Represents the gap between the claim (authentic Zoroastrian law) and the reality (colonial-era codification).
narrative_ontology:constraint_stakeholder(parsi_community_reading, parsi_marriage_and_divorce_act_1936, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Abstract collective good that benefits from endogamy enforcement and community marriage authority. Cannot organize or exit. Depends on the constraint for transmission of religious identity and cultural practice. Vindicated by the constraint's operation but not an agent in the conventional sense.
narrative_ontology:constraint_stakeholder(parsi_community_reading, zoroastrian_cultural_continuity, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(parsi_community_reading, zoroastrian_cultural_continuity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates marriage recognition, inheritance rights, and religious identity transmission within the Parsi community. It solves the genuine coordination problem: how does a minority religious community maintain identity and cultural continuity in a pluralistic state? The coordination function is real — the community uses the constraint to recognize marriages, transmit religious practice, and maintain kinship structures.
% TRANSFER_FUNCTION: The constraint transfers authority from the Indian secular state to the Parsi community institutional authority. It also transfers status from non-Parsi spouses to Parsi spouses (status asymmetry). It transfers identity from exogamous Parsi individuals to the community (identity loss for those who marry outside). The primary transfer is authority over marriage recognition; the secondary transfers are status and identity.
% ABSENT_VOICES: Parsi women who have married non-Parsi partners and experienced status loss are partially excluded from community authority structures (they may not serve on community councils in some cases). Secular Parsi individuals who reject the constraint's legitimacy are not represented in community decision-making. Non-Parsi spouses have no voice in the constraint's formulation or enforcement. These absent voices would object to endogamy enforcement and status asymmetry.
% DISAPPEARANCE_RATIONALE: The Parsi community institutional authority argues that if the constraint disappeared, the community would rearrange itself around secular marriage law, losing cultural continuity and religious identity transmission (world_rearranges). Secular Parsi individuals argue that if the constraint disappeared, the community would continue to exist and practice religion without formal marriage authority (world_unchanged). The contest reflects disagreement about whether the constraint is essential to community identity or merely one institutional form among alternatives.
% FOUNDING_PROBLEM: The founding problem (1936) was: how can the Parsi community maintain Zoroastrian identity and cultural continuity in a pluralistic Indian state where secular marriage law is available? The constraint was built to enforce endogamy and maintain community boundaries through formal legal authority.
% FOUNDING_PROBLEM_CORROBORATION: The Parsi community institutional authority attests that the founding problem remains live — endogamy enforcement is necessary for cultural continuity. Secular Parsi individuals and constitutional rights advocates argue that the founding problem is dead — cultural continuity can be maintained without formal marriage authority, and endogamy enforcement violates fundamental rights. The contest reflects genuine disagreement about whether the founding problem persists.
narrative_ontology:disappearance_verdict(parsi_community_reading, contested).
narrative_ontology:founding_problem_status(parsi_community_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PARSI INDIVIDUAL CHOOSING EXOGAMY (SNARE) — Structurally mobile (can legally marry outside community) but identity-locked through internalized community belonging and family pressure. Exit from the constraint (marrying non-Parsi) requires abandoning identity as a full community member. The binding is cognitive/relational rather than legal, but the cost is existential — loss of community status, family rupture, exclusion from religious ceremonies. Experiences maximum extraction: the constraint forces a choice between romantic autonomy and identity continuity.
constraint_indexing:constraint_classification(parsi_community_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-PARSI SPOUSE (TANGLED ROPE) — Faces genuine coordination problem (marriage requires community recognition) alongside asymmetric extraction (loses community status, children's religious identity contested, excluded from certain ceremonies). The constraint both coordinates the marriage and extracts from the non-Parsi partner through status loss. Exit is possible (secular marriage) but carries relational costs. Moderate experienced extraction — some agency but significant asymmetry.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARSI COMMUNITY INSTITUTIONAL AUTHORITY (ROPE) — Benefits from the constraint through institutional authority, cultural continuity, and endogamy enforcement. Experiences the constraint as coordination: maintaining community boundaries enables religious practice transmission and cultural preservation. Net beneficiary with high exit optionality (can modify rules, can arbitrage between community law and secular law). Low experienced extraction — the constraint serves the institution's core function.
constraint_indexing:constraint_classification(parsi_community_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDIAN CONSTITUTIONAL STATE (TANGLED ROPE) — Coordinates religious community autonomy (recognizes Parsi personal law) while extracting through judicial oversight and constitutional constraint (marriage must not violate fundamental rights; state retains authority to invalidate discriminatory provisions). The state both enables community self-governance and limits it. Constrained exit (cannot fully withdraw recognition without constitutional crisis) but significant agency in setting boundaries. Moderate extraction — the state benefits from community self-administration while bearing costs of adjudicating conflicts.
constraint_indexing:constraint_classification(parsi_community_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COLONIAL-ERA LEGAL FRAMEWORK (PITON) — The Parsi Marriage and Divorce Act (1936) is a codified, formalized constraint that persists through institutional inertia. The framework is largely performative: it claims to represent Zoroastrian tradition but actually represents a colonial-era codification that froze evolving community practice. The constraint persists because alternatives (full secular marriage, full religious autonomy) haven't fully replaced it, not because the framework functionally serves its stated purpose. Theater ratio reflects the gap between the claim (authentic Zoroastrian law) and the reality (colonial-era codification). Piton classification derives from degraded function maintained theatrically.
constraint_indexing:constraint_classification(parsi_community_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of marriage authority is inherent to any social order: every society must coordinate marriage recognition, inheritance, and kinship. The constraint appears as an immutable feature of social organization itself. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that what appears as natural law is actually a contingent institutional choice (Zoroastrian community authority vs. secular state authority vs. Hindu personal law vs. Islamic shariat).
constraint_indexing:constraint_classification(parsi_community_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parsi_community_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parsi_community_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parsi_community_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(parsi_community_reading, TR),
    TR >= 0.70.

:- end_tests(parsi_community_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint extracts from non-Parsi spouses (status loss, religious identity contestation) and from Parsi individuals choosing exogamy (identity rupture, family pressure). However, the extraction is not severe because: (1) secular marriage alternatives exist with low legal barriers; (2) the community's enforcement mechanisms are primarily informal (social pressure, family rupture) rather than legal coercion; (3) the constraint's primary function is coordination (marriage recognition) rather than pure extraction. The modest increase over the interval (0.28 → 0.35) reflects gradual intensification of identity-lock mechanisms as secular alternatives become more available — the constraint must work harder to maintain endogamy as legal barriers fall. Suppression (0.42): Moderate. Significant barriers to exit include identity fusion (internalized community belonging), family pressure, and status loss. However, suppression is not total because secular marriage is legally available and increasingly normalized. The declining trajectory (0.50 → 0.42) reflects erosion of suppression mechanisms as constitutional rights frameworks and secular norms reduce the cost of exogamy. Theater ratio (0.38): Moderate-low. The constraint's performative content is lower than the piton perspective suggests because the coordination function is genuine — the community does actually recognize marriages, does transmit religious identity, does maintain kinship structures. The theater reflects the gap between the claim (authentic Zoroastrian law) and the reality (colonial-era codification), but the gap is not as wide as in purely performative constraints. The stable trajectory reflects that the constraint's performative content is stable — the codification remains formally valid even as its functional authority declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of classification from different structural positions. The Parsi individual choosing exogamy sees snare (maximum extraction, identity lock). The non-Parsi spouse sees tangled rope (mixed coordination and extraction). The community institutional authority sees rope (coordination, net benefit). The Indian state sees tangled rope (mixed coordination and constraint). The colonial-era framework sees piton (degraded function maintained theatrically). The civilizational analytical observer risks seeing mountain (natural law of social organization) but the structural data reveals this as a false summit: the constraint is a contingent institutional choice, not an immutable feature of marriage itself. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural position relative to the endogamy enforcement mechanism and the identity-lock binding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. The Parsi community institutional authority has d ≈ 0.0 (full beneficiary: benefits from authority, cultural continuity, endogamy enforcement). The Parsi individual choosing exogamy has d ≈ 1.0 (full target: bears extraction through identity lock and family pressure). The non-Parsi spouse has d ≈ 0.8 (strong target: bears status loss and religious identity contestation). The Indian state has d ≈ 0.5 (symmetric: coordinates community autonomy while constraining through rights; benefits from community self-administration while bearing costs of adjudication). The colonial-era framework has d ≈ 0.2 (weak beneficiary: maintains formal authority but with declining functional force). These directionality values feed the engine's effective extraction computation (χ), which scales extractiveness by directionality and scope. The identity_locked exit option for the Parsi individual choosing exogamy is critical: it indicates that the binding mechanism is cognitive (identity fusion) rather than structural (legal barriers), which affects how the engine models the agent's experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The Parsi reading resolves mandatrophy by clarifying that the constraint's mandate (maintain Zoroastrian community identity through marriage authority) has NOT outlived its function — the community continues to use the constraint for cultural continuity and religious practice transmission. However, the constraint exhibits signs of mandate drift: the original mandate (enforce endogamy to preserve Zoroastrian identity) is increasingly contested by secular alternatives and constitutional rights frameworks. The constraint persists not because the mandate is universally accepted but because the community institutional authority maintains it through formal authority and informal enforcement. The piton perspective captures this drift: the constraint is increasingly performative (maintains formal authority while functional force declines). The tangled rope classification reflects that the constraint still serves genuine coordination (marriage recognition) alongside extraction (endogamy enforcement, status asymmetry). The false summit mountain perspective reveals the risk of naturalizing this contingent institutional choice as immutable law — the constraint is a choice, not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is Parsi marriage authority grounded in immutable Zoroastrian religious law, or is it a colonial-era codification that froze evolving community practice?',
    'Historical analysis of pre-1936 Parsi marriage practices; comparison with contemporary Zoroastrian communities outside India; textual analysis of the 1936 Act''s relationship to classical Zoroastrian jurisprudence',
    'If natural law: mountain classification holds; community authority is inherent to Zoroastrian identity. If constructed: false summit detected; the constraint is a contingent institutional arrangement that benefits the codifying authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, conceptual, 'Whether Parsi marriage authority is natural law or colonial codification').

omega_variable(
    identity_lock_mechanism,
    'Is the binding mechanism for Parsi individuals choosing exogamy structural (legal barriers, economic dependency) or internalized (identity fusion, family pressure, community belonging)?',
    'Post-exit trajectory analysis: do Parsi individuals who marry non-Parsi partners experience persistent suppression after legal exit, or does suppression terminate with the marriage? Qualitative interviews with exogamous Parsi individuals about identity continuity.',
    'If structural: reclassify to trapped exit. If internalized: identity_locked classification confirmed; the constraint''s binding is cognitive rather than legal. If mixed: declare the proportion and model suppression as partially portable (the agent carries internalized suppression after exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether identity lock is structural or internalized').

omega_variable(
    endogamy_enforcement_mechanism,
    'What enforces endogamy — formal community sanctions (exclusion from ceremonies, inheritance loss), informal social pressure (family rupture, status loss), or internalized identity commitment (self-policing through identity fusion)?',
    'Ethnographic documentation of enforcement practices; interviews with community authorities about sanctions; analysis of actual enforcement patterns vs. stated rules',
    'If formal: suppression metric should be higher (0.42 → 0.55+). If informal: suppression is real but not legally codified; the constraint''s extractiveness may be lower than the institutional authority claims. If internalized: the constraint is more extractive than formal enforcement alone would suggest (identity lock amplifies suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_enforcement_mechanism, empirical, 'Mechanism of endogamy enforcement').

omega_variable(
    kernel_reading_contest,
    'Which reading of the marriage authority kernel is institutionally dominant in contemporary Parsi community governance — the Zoroastrian codified reading, the secular contractual reading, or a hybrid?',
    'Analysis of actual marriage dispute adjudication: which authority do Parsi individuals appeal to (community courts, secular courts, or both)? What proportion of Parsi marriages are solemnized under the 1936 Act vs. secular law? Interviews with community authorities about their perceived legitimacy.',
    'If Zoroastrian reading dominant: the constraint''s institutional authority is secure. If secular reading dominant: the Zoroastrian reading is a residual authority maintained for cultural continuity but not functionally primary (piton classification confirmed). If hybrid: the constraint is in transition; the reading_relations may need revision (influences rather than coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which reading of marriage authority is institutionally dominant').

omega_variable(
    constitutional_constraint_evolution,
    'How has the Indian Constitution''s fundamental rights framework (Articles 14, 15, 21) constrained the Parsi Marriage and Divorce Act over time? Is the constraint evolving toward secular convergence?',
    'Longitudinal analysis of court decisions on Parsi marriage disputes; tracking of constitutional challenges to the 1936 Act; comparison with evolution of Hindu personal law under constitutional pressure',
    'If constitutional pressure is strong: the constraint is drifting toward secular convergence; the state''s tangled_rope classification may be shifting toward snare (the state is increasingly extracting authority from the community). If weak: the constraint is stable; the state''s arbitrage exit is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_constraint_evolution, empirical, 'Constitutional constraint on Parsi marriage authority').

omega_variable(
    sibling_reading_foreclosure,
    'Does the Parsi Zoroastrian codified reading logically foreclose the secular contractual reading, or do they coexist as live options held by different parties?',
    'Logical analysis of the core premises: does Zoroastrian authority claim exclusive legitimacy, or does it permit secular marriage as an alternative? Empirical analysis: do Parsi individuals hold both readings simultaneously (e.g., religious marriage + secular registration)?',
    'If foreclosure: the readings are in genuine conflict; one must be overridden. If coexistence: both readings are live; the constraint family exhibits genuine pluralism. If influences: the Zoroastrian reading creates pressure on the secular reading but doesn''t rule it out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether Zoroastrian reading forecloses secular reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parsi_community_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(parsi_theater_1936, parsi_community_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(parsi_theater_1966, parsi_community_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(parsi_theater_1996, parsi_community_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(parsi_theater_2026, parsi_community_reading, theater_ratio, 90, 0.38).

% Extraction over time
narrative_ontology:measurement(parsi_extractiveness_1936, parsi_community_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(parsi_extractiveness_1966, parsi_community_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(parsi_extractiveness_1996, parsi_community_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(parsi_extractiveness_2026, parsi_community_reading, base_extractiveness, 90, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(parsi_suppression_1936, parsi_community_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(parsi_suppression_1966, parsi_community_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(parsi_suppression_1996, parsi_community_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(parsi_suppression_2026, parsi_community_reading, suppression_requirement, 90, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parsi_community_reading, identity_coordination).
narrative_ontology:affects_constraint(parsi_community_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(parsi_community_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(parsi_community_reading, christian_colonial_reading).
narrative_ontology:affects_constraint(parsi_community_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The marriage authority kernel decomposes into five structurally distinct constraints, each with its own ε value, beneficiary/victim structure, and classification. The Parsi reading (this story) has moderate extractiveness (0.35) and tangled rope classification. The secular reading has lower extractiveness and rope classification. The Hindu and Muslim readings have higher extractiveness and tangled rope or snare classifications. The Christian reading is historically residual (piton classification). Each reading is a complete constraint story; the network links show how they compete for institutional authority within the pluralistic Indian constitutional framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parsi_community_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
