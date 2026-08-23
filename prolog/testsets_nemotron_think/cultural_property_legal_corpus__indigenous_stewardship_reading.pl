% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Indigenous Stewardship Reading of Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This constraint story captures the indigenous stewardship reading of the
 *   international cultural property legal corpus. The standing arrangement —
 *   museums and successor states holding legal title to artifacts acquired
 *   through colonial violence — is assessed from the reading's own lights:
 *   the constraint extracts cultural authority, economic value, and epistemic
 *   control from indigenous communities while coordinating preservation and
 *   access for global audiences. The reading claims legitimate authority
 *   rests with communities maintaining cultural continuity, not with the
 *   legal heirs of colonial extraction. This is one of three contested
 *   readings of the cultural_property_legal_corpus kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.85).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.78).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Indigenous Stewardship Reading of Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0').
narrative_ontology:cs_kernel_codification('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', formalized).
narrative_ontology:cs_authority_grounding('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', extraction).
narrative_ontology:cs_interpretation_layer_present('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0').
narrative_ontology:cs_reading_relation('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', foundational, cultural_artifacts_are_communal_property).
narrative_ontology:cs_axiom_status(cultural_artifacts_are_communal_property, holdable).
narrative_ontology:cs_axiom_grounding('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', cultural_artifacts_are_communal_property, deontological).
narrative_ontology:cs_axiom('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', foundational, legitimate_authority_requires_cultural_continuity).
narrative_ontology:cs_axiom_status(legitimate_authority_requires_cultural_continuity, holdable).
narrative_ontology:cs_axiom_grounding('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', legitimate_authority_requires_cultural_continuity, deontological).
narrative_ontology:cs_reference_frame('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', colonial_legal_continuity).
narrative_ontology:cs_drift_state('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', contemporary_repatriation_movements, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('41b4ca74-6958-4fe8-a3e4-479c0a8ca4b0', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, auction_houses).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, global_public).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_as_legitimate_authority).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, communal_property_over_state_ownership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sacred and communal relationship to artifacts removed under colonial violence. Current legal frameworks deny them standing to reclaim; they must petition successor states or museums that hold title. Exit means abandoning cultural continuity — identity is fused to the artifacts and the lands from which they were taken. Repatriation claims face procedural barriers, statute of limitations defenses, and demands for 'proof' that fits Western evidentiary standards.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, payer,
    powerless, generational, identity_locked, local).

% Communities with direct genealogical and cultural continuity to the artifacts but lacking formal state recognition or international legal personality. They bear the cultural loss without the procedural tools of recognized indigenous peoples. Their claims are often mediated through or subsumed by successor state narratives.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_communities, excluded).

% Hold legal title and physical possession of vast collections acquired during colonial eras. They set conservation standards, access terms, and loan conditions. They benefit from prestige, tourism revenue, research primacy, and the authority to define what counts as 'provenance.' Their exit options are maximal — they can deaccession, repatriate selectively, or resist claims while citing universal preservation mandates. The same institutions often adjudicate the standards by which claims are judged.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, museums, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, museums, beneficiary).

% Post-colonial nation-states that inherited colonial borders and legal systems. They claim sovereign ownership of cultural heritage within their territory, often invoking the same colonial legal doctrines they otherwise reject. They benefit from nationalist legitimacy, UNESCO nominations, and control over export permits. They can negotiate government-to-government repatriation but rarely cede authority to sub-state indigenous communities. Their exit is mobile — they can change domestic law, but the international framework incentivizes state-centric control.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, beneficiary).

% Former colonial powers' museums, universities, and cultural bodies that retain collections and the epistemic authority to interpret them. They benefit from continued research access, curatorial careers, and the soft power of 'universal museums.' Their exit is constrained by domestic politics and international pressure but they retain structural advantages in funding, expertise, and legal frameworks they helped write.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_institutions, beneficiary,
    organized, biographical, constrained, global).

% Commercial intermediaries that profit from the market in cultural artifacts. They benefit from legal opacity around provenance, the difficulty of proving illicit origin, and the high value placed on 'unprovenanced' masterpieces. They face minimal exit costs — they can shift jurisdictions, use freeports, or rebrand. Their role is extractive without coordination function; they are pure rent-takers on the constraint's ambiguity.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, auction_houses, beneficiary,
    powerful, immediate, arbitrage, global).

% UNESCO, UNIDROIT, WIPO, and treaty bodies that administer the 1970 and 1995 conventions. They produce soft law, facilitate dialogue, and register state parties. They do not collect extraction directly but their frameworks legitimize the state-centric model. Their analytical seat is compromised by funding dependence on member states and the procedural impossibility of recognizing non-state claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Museum visitors, students, and digital audiences who access artifacts in major institutions. They receive genuine coordination benefit — education, aesthetic experience, cross-cultural exposure — but this access is predicated on the artifacts' removal. Their exit is mobile (they can visit other museums, view digital surrogates) and they bear no direct cost. They are incidental beneficiaries of the extraction, not its architects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, global_public, beneficiary,
    organized, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for preserving, documenting, and publicly displaying cultural artifacts across borders, preventing destruction and enabling scholarly study.
% TRANSFER_FUNCTION: Moves legal title, physical possession, epistemic authority, and economic value of cultural artifacts from indigenous and descendant communities to museums, successor states, and market intermediaries. The transfer is enforced through colonial-era property doctrines, statute of limitations, state immunity, and the evidentiary burden placed on claimants.
% ABSENT_VOICES: Indigenous communities without state recognition, displaced peoples without territorial continuity, and spiritual practitioners whose authority derives from ceremonial knowledge rather than legal personhood. They are absent because the international legal system recognizes only states and state-chartered institutions as subjects. Their exclusion is structural — the constraint's architecture has no seat for them.
% DISAPPEARANCE_RATIONALE: If the current legal corpus vanished overnight, artifacts would not automatically return — but the legal barriers to repatriation would disappear. Museums would lose title defenses; successor states would lose sovereign immunity shields; auction houses would lose provenance laundering mechanisms. Negotiation would shift from legal proceduralism to political and moral reckoning. The world of cultural property would reorganize around physical custody, cultural continuity, and the capacity to care — not colonial legal title.
% FOUNDING_PROBLEM: Colonial powers needed a legal framework to legitimize the mass removal of cultural artifacts from conquered territories while presenting it as preservation, science, and civilization. The 19th-century museum model and the post-WWII international conventions were built to manage colonial collections, not to address the claims of the peoples from whom they were taken.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous testimony at UN Permanent Forum on Indigenous Issues; post-colonial legal scholarship (e.g., Meskell, Nicholas, Torsen); the 2007 UNDRIP drafting history where states resisted 'free, prior, and informed consent' for cultural heritage. No corroborating source outside the beneficiary set affirms the founding problem remains live in its original terms — even UNESCO now acknowledges the colonial origins of its own conventions.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is very high (0.85) because the constraint's operation transfers the core value of the artifacts — cultural authority, spiritual integrity, communal identity — from communities that sustain them to institutions that monetize and academize them. Suppression (0.78) reflects the legal barriers: statute of limitations, state immunity, burden of proof on claimants, non-retroactivity of conventions. Theater ratio (0.42) has risen as museums adopt 'decolonial' rhetoric and provenance research while retaining possession — the coordination function (preservation, access) is real but increasingly performed to legitimize continued extraction. Accessibility collapse (0.76) is high because indigenous communities cannot practically exit the constraint: their cultural continuity depends on artifacts they cannot access, and the legal system offers no viable alternative path. Resistance (0.62) is substantial and growing — repatriation movements, UNDRIP, digital rematriation, and direct action — but meets institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the indigenous community seat, the constraint is a snare: pure extraction disguised as preservation law. From the museum seat, it is a rope: genuine coordination of global heritage stewardship. From the successor state seat, it is a tangled rope: they coordinate national heritage while extracting from sub-state communities. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the reading's assessment that coordination and extraction are inseparable in the current corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are identity-locked targets (d ≈ 0.95): their cultural self-concept is constituted through the artifacts, making exit existentially costly. Descendant communities are constrained payers (d ≈ 0.8) — they have some mobility but lack legal personality. Museums are agenda-setters with arbitrage exit (d ≈ 0.1) — they set rules and can exit by deaccessioning. Successor states are institutional agenda-setters with mobile exit (d ≈ 0.2) — they control domestic law but are locked into the international framework. Colonial institutions are organized beneficiaries (d ≈ 0.25). Auction houses are powerful beneficiaries with arbitrage exit (d ≈ 0.05). International legal bodies are analytical observers (d ≈ 0.5). Global public are organized beneficiaries with mobile exit (d ≈ 0.3) — they benefit from access but bear no enforcement cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimizing colonial collections — is dead in its original form but the arrangement persists. The constraint now serves a different function: maintaining the asset base of major museums and the sovereign claims of successor states. This is classic mandatrophy: the mandate (colonial legitimation) has atrophied but the constraint (state-centric cultural property law) remains because no beneficiary is hurt enough to dismantle it and no victim has the power to force revision. The rising theater ratio tracks the performance of 'decolonial museology' that changes labels but not title.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the cultural_property_legal_corpus a single constraint with contested interpretation, or are the three readings structurally distinct constraints sharing only a label?',
    'Apply ε-invariance test: if measuring extraction under the universal_heritage reading yields low ε (museums as coordinators) but under indigenous_stewardship reading yields high ε (museums as extractors), they are distinct constraints. The kernel label conflates them.',
    'If distinct constraints, each reading gets its own story with its own ε, stakeholders, and classification. The current story correctly models only the indigenous_stewardship reading. The universal_heritage and sovereign_repatriation readings require separate constraint stories linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel label names one constraint or a family of structurally distinct constraints.').

omega_variable(
    preservation_requires_removal,
    'Does effective preservation of fragile artifacts structurally require their removal from source communities and concentration in metropolitan institutions?',
    'Compare conservation outcomes for repatriated artifacts under community care vs. museum care. Examine cases where source communities have built climate-controlled facilities with international support.',
    'If preservation does not require removal, the coordination function of the current corpus is separable from its extraction — supporting tangled_rope. If preservation genuinely requires metropolitan concentration, part of the measured extraction is the price of coordination, and the constraint is more rope-like under the universal_heritage reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_requires_removal, empirical, 'Whether the coordination function (preservation) is structurally coupled to the extraction (removal from source communities).').

omega_variable(
    state_as_proxy_for_communities,
    'Can successor states legitimately act as proxies for indigenous communities in cultural property claims, or does state sovereignty structurally conflict with communal cultural authority?',
    'Track repatriation outcomes where states negotiate on behalf of communities: do artifacts return to community control or to state museums? Analyze domestic heritage laws for community decision-making power.',
    'If states consistently capture repatriated artifacts for national museums, the sovereign_repatriation_reading and indigenous_stewardship_reading are in structural conflict (forecloses). If states devolve authority to communities, they coexist_with. This determines the reading_relation between this story and the sovereign_repatriation_reading story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_as_proxy_for_communities, empirical, 'Whether state sovereignty and indigenous cultural authority are compatible proxies or structural rivals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of indigenous claims structural (legal barriers, state immunity) or internalized (communities accepting museums as legitimate custodians, epistemic capture)?',
    'Post-repatriation trajectories: if communities that regain artifacts still seek museum partnerships for conservation, suppression was partly structural. If communities reject all museum frameworks, suppression was internalized epistemic capture.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint operates through identity formation, not just law. This would increase χ for identity_locked agents beyond the engine''s structural derivation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in cultural property law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplc_isr_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cplc_isr_tr_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cplc_isr_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(cplc_isr_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(cplc_isr_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cplc_isr_tr_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(cplc_isr_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.92).
narrative_ontology:measurement(cplc_isr_be_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 10, 0.88).
narrative_ontology:measurement(cplc_isr_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.85).
narrative_ontology:measurement(cplc_isr_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(cplc_isr_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(cplc_isr_be_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(cplc_isr_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(cplc_isr_su_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(cplc_isr_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(cplc_isr_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(cplc_isr_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(cplc_isr_su_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.08).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, unesco_1970_convention_operationalization).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, national_patrimony_laws).

% DUAL FORMULATION NOTE:
% This story is one member of the cultural_property_legal_corpus constraint family. The three readings decompose the kernel into structurally distinct constraints with different ε values, beneficiary/victim structures, and claimed types. The indigenous_stewardship_reading has the highest ε (0.85) because under this reading the standing arrangement extracts from the communities with the strongest cultural continuity claim. The universal_heritage_reading would have lower ε (museums as genuine coordinators). The sovereign_repatriation_reading would have intermediate ε (states as both coordinators and extractors vis-à-vis sub-state communities). All three stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, powerless, 0.95).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, moderate, 0.8).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.15).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, powerful, 0.05).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, organized, 0.25).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
