% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Mandate for Colonial-Era Cultural Artifacts
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This constraint instantiates the sovereign repatriation reading of the
 *   cultural property legal corpus kernel. It asserts that colonial-era
 *   acquisition of cultural artifacts was illegitimate extraction, and that
 *   legitimate authority over those artifacts rests exclusively with
 *   successor states claiming historical continuity with the expropriated
 *   peoples. The constraint operates through international conventions
 *   (UNESCO 1970, UNIDROIT 1995), bilateral treaties, domestic cultural
 *   patrimony laws, and customary international law development. It
 *   coordinates the return of artifacts while extracting compliance costs
 *   from holding institutions — a genuine tangled rope where the coordination
 *   function (lawful return) and extraction function (institutional loss of
 *   collections, authority, and revenue) are structurally fused.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.35).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Mandate for Colonial-Era Cultural Artifacts").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'b05ca328-4452-433a-bd0b-409ea9137fe6').
narrative_ontology:cs_kernel_codification('b05ca328-4452-433a-bd0b-409ea9137fe6', formalized).
narrative_ontology:cs_authority_grounding('b05ca328-4452-433a-bd0b-409ea9137fe6', lineage).
narrative_ontology:cs_interpretation_layer_present('b05ca328-4452-433a-bd0b-409ea9137fe6').
narrative_ontology:cs_reading_relation('b05ca328-4452-433a-bd0b-409ea9137fe6', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('b05ca328-4452-433a-bd0b-409ea9137fe6', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('b05ca328-4452-433a-bd0b-409ea9137fe6', foundational, state_continuity_grounds_cultural_sovereignty).
narrative_ontology:cs_axiom_status(state_continuity_grounds_cultural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b05ca328-4452-433a-bd0b-409ea9137fe6', state_continuity_grounds_cultural_sovereignty, conventional).
narrative_ontology:cs_axiom('b05ca328-4452-433a-bd0b-409ea9137fe6', foundational, colonial_acquisition_creates_no_valid_title).
narrative_ontology:cs_axiom_status(colonial_acquisition_creates_no_valid_title, holdable).
narrative_ontology:cs_axiom_grounding('b05ca328-4452-433a-bd0b-409ea9137fe6', colonial_acquisition_creates_no_valid_title, deontological).
narrative_ontology:cs_reference_frame('b05ca328-4452-433a-bd0b-409ea9137fe6', post_colonial_legal_order_1970).
narrative_ontology:cs_drift_state('b05ca328-4452-433a-bd0b-409ea9137fe6', contemporary_reparative_turn, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b05ca328-4452-433a-bd0b-409ea9137fe6', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples_claiming_state_continuity).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions_in_former_metropoles).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors_with_contested_provenance).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, auction_houses_handling_disputed_objects).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_acquisition_illegitimacy_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_sovereignty_over_cultural_patrimony).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, historical_continuity_grounds_legitimate_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that claim direct historical continuity with peoples expropriated during colonial rule. They initiate repatriation claims through diplomatic channels and international courts, bear the cost of building legal cases, and receive returned artifacts as sovereign property. Their identity is fused to the recovery of patrimony — exit from the repatriation frame would constitute a repudiation of their founding legitimacy.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity, agenda_setter).

% Communities within successor states whose specific lineages, languages, and ceremonial practices were disrupted by artifact removal. They benefit symbolically and politically from returns but do not control the diplomatic machinery. Their identity is bound to the state's repatriation performance — they cannot exit the claim without exiting the state's national narrative.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, expropriated_peoples_claiming_state_continuity, beneficiary,
    moderate, biographical, identity_locked, national).

% Major museums and cultural institutions in former colonial powers (e.g., British Museum, Louvre, Humboldt Forum, Metropolitan Museum). They hold contested collections, fund provenance research, negotiate returns, and lose objects and institutional authority when claims succeed. Their exit is constrained by legal mandates, public trust doctrine, and the reputational cost of refusal — they cannot simply walk away from their collections.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions_in_former_metropoles, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions_in_former_metropoles, agenda_setter).

% Individuals and family trusts holding artifacts with gaps in ownership history 1850–1970. They face seizure risk, devaluation, and legal liability. Their exit is relatively mobile — they can sell into grey markets, donate to non-claimant institutions, or hold until statutes of limitation evolve — but each exit path carries escalating cost.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors_with_contested_provenance, payer,
    powerful, biographical, mobile, global).

% Sotheby's, Christie's, Bonhams and regional houses that intermediate sales of objects with contested provenance. They bear due-diligence costs, reputational risk, and occasional repatriation claims post-sale. Their exit is constrained by the market's increasing demand for clean provenance — they cannot credibly operate without compliance infrastructure.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, auction_houses_handling_disputed_objects, payer,
    organized, immediate, constrained, global).

% Indigenous nations and communities whose artifacts were taken but who do not have or seek state-level recognition. Their claims are routed through successor states or dismissed as lacking standing in state-centric international law. They are excluded from the diplomatic table where repatriation terms are negotiated.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities_outside_state_framework, excluded,
    powerless, biographical, trapped, local).

% Standard-setting bodies (UNESCO 1970 Convention, UNIDROIT 1995, ICOM Red Lists) that frame the legal vocabulary but do not adjudicate specific returns. They observe the constraint's operation across jurisdictions and publish normative guidance.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_and_international_cultural_heritage_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a state-centric legal framework for returning cultural patrimony removed under colonial rule, converting moral claims into enforceable diplomatic and judicial processes. Solves the coordination problem of which polity holds legitimate title when the original polity no longer exists as a sovereign actor.
% TRANSFER_FUNCTION: Moves physical artifacts and legal title from holding institutions (mostly in former metropoles) to successor states claiming historical continuity. Transfers symbolic capital (legitimacy, sovereignty performance) to claimant states; transfers financial and reputational costs to holding institutions. Moves identity capital from colonial-era collections to post-colonial national narratives.
% ABSENT_VOICES: Indigenous communities outside the state framework (see excluded stakeholder) — their artifacts are claimed by states they may not recognize as legitimate representatives. Diaspora communities severed from both the artifact's origin and the successor state. Source-country scholars and curators who argue for shared custody or loan frameworks rather than permanent transfer.
% DISAPPEARANCE_RATIONALE: If the sovereign repatriation mandate vanished overnight, holding institutions would retain contested collections indefinitely, successor states would lose their primary legal lever for recovery, and the moral grammar of 'colonial illegitimacy' would lose its enforcement mechanism. The artifact landscape would freeze in its colonial configuration.
% FOUNDING_PROBLEM: After decolonization (1940s–1970s), newly independent states found their cultural patrimony physically located in former imperial metropoles with no legal mechanism for recovery. The colonial legal order had legitimized acquisition; the post-colonial order needed a counter-doctrine to make return legally cognizable.
% FOUNDING_PROBLEM_CORROBORATION: Successor states and UNESCO attest the problem remains live — thousands of objects remain unreturned, and new categories of contested material (human remains, digital surrogates, intangible heritage) keep emerging. Holding institutions and some legal scholars attest the founding problem is substantially solved for the core canon (high-profile returns have established precedent), and the constraint now functions as leverage for broader cultural diplomacy. No independent corroboration exists outside these two camps — the 'contested' status reflects genuine disagreement over whether the mandate has achieved its purpose.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).
:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.42) reflects moderate but real costs: legal fees, provenance research, physical transfer, loss of visitor revenue, and erosion of 'universal museum' authority. Suppression (0.35) is present but not overwhelming — holding institutions retain significant discretion (loan frameworks, shared custody, digital repatriation) and the constraint does not criminalize possession outright. Theater ratio (0.28) captures the growing performative dimension: high-profile returns staged for diplomatic signaling while vast reserves remain unexamined. Accessibility collapse (0.65) is moderately high — once a state adopts the sovereign repatriation frame, alternative frameworks (universal heritage, shared custody) become diplomatically costly to entertain. Resistance (0.55) is substantial: holding institutions deploy legal, technical, and moral arguments to delay or limit returns.
 *
 * PERSPECTIVAL GAP:
 *   The successor state seat experiences this as coordination (restoring sovereignty) with minimal extraction. The holding institution seat experiences it as extraction (losing collections, authority, revenue) with coordination as cover. The indigenous community seat (excluded) experiences it as a new form of dispossession — their artifacts claimed by a state they may not recognize. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) captures the authoring seat's judgment that both functions are real and fused.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are structural beneficiaries — they receive artifacts and symbolic capital, and their identity is fused to the claim (identity_locked exit). Holding institutions are structural payers — they bear costs and lose authority, but their exit is constrained rather than trapped (they remain legitimate cultural actors). Private collectors and auction houses are payers with more exit mobility (mobile/constrained). Indigenous communities outside the state framework are excluded — their claims are structurally invisible in this reading. UNESCO and international bodies are analytical observers. The directionality derives from beneficiary/victim declarations plus exit options: identity_locked beneficiaries sit near d=0 (subsidized), constrained payers sit near d=0.7-0.8, mobile payers sit lower.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-colonial legal vacuum for cultural return) was real and the mandate solved it for a generation. But the constraint now extracts from holding institutions beyond what the coordination function requires — the theater ratio rising from 0.1 to 0.28 shows performative returns substituting for systematic provenance resolution. The mandate has not fully atrophied (returns still occur, new claims emerge), but it shows piton drift: the coordination machinery runs partly to maintain the appearance of justice while the hardest cases stall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_vs_community_continuity,
    'Does historical continuity with expropriated peoples legitimately run through the successor state, or does it run through communities maintaining cultural practice regardless of state boundaries?',
    'Comparative analysis of repatriation outcomes where successor states and indigenous communities make competing claims for the same artifacts (e.g., Benin Bronzes: Nigeria state vs. Edo communities; Maori taonga: New Zealand state vs. iwi). Track which claims hold legal weight and which communities receive artifacts post-return.',
    'If continuity runs through communities, the sovereign repatriation reading extracts identity capital from the very peoples it claims to restore — a structural misalignment that would reclassify the constraint toward snare for the community seat. If continuity runs through states, the reading''s coordination function is validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_vs_community_continuity, conceptual, 'Whether state continuity or community continuity is the legitimate carrier of historical authority.').

omega_variable(
    universal_heritage_incompatibility,
    'Is the sovereign repatriation frame logically incompatible with the universal heritage frame, or can they coexist as layered principles (return for some objects, shared custody for others)?',
    'Legal analysis of treaty interpretation: whether UNESCO 1970 and UNIDROIT 1995 permit universal heritage exceptions to repatriation obligations, and whether state practice treats them as alternative regimes or hierarchical principles.',
    'If forecloses, the two readings cannot operate in the same legal framework — a state must choose one as its governing principle. If coexists_with or influences, hybrid regimes (return of sacred objects, loans for others) are structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_incompatibility, conceptual, 'Logical relationship between sovereign repatriation and universal heritage as governing principles.').

omega_variable(
    extraction_ceiling_for_holding_institutions,
    'At what point does the cumulative cost of repatriation (financial, reputational, authoritative) exceed the coordination benefit of the legal framework for holding institutions?',
    'Longitudinal tracking of institutional responses: when do major museums shift from case-by-case negotiation to blanket refusal, policy reform advocacy, or structural divestment? Correlation of return volume with institutional metrics (visitors, funding, research output).',
    'If extraction ceiling is reached, holding institutions become organized resistance rather than constrained payers — the constraint''s enforcement mechanism degrades, potentially shifting classification toward piton (theatrical compliance) or snare (coercion without coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_ceiling_for_holding_institutions, empirical, 'Whether holding institutions'' extraction tolerance has a structural limit that would destabilize the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.38).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2018, 0.42).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1985, 0.25).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.33).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.08).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the cultural_property_legal_corpus kernel. The universal_heritage_reading locates authority in preserving institutions; the indigenous_stewardship_reading locates authority in communities of cultural continuity. All three claim the same artifact set but with different beneficiary/victim structures and different ε values. They form a constraint family linked by shared referent and contested legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, institutional, 0.15).
constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
