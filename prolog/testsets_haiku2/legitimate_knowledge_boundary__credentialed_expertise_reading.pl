% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Gatekeeping (Legitimate Knowledge Boundary)
 *   domain: epistemology/science_and_technology_studies
 *
 * SUMMARY:
 *   A constraint that defines legitimate knowledge as the product of
 *   methodologically rigorous inquiry validated through credentialed peer
 *   review. This reading treats methodology and credentialing as the primary
 *   epistemic gatekeeping mechanisms. The constraint provides genuine
 *   coordination value — it aggregates validation across dispersed
 *   researchers and enables trust in knowledge without individual
 *   re-verification. But it also operates as a form of institutional
 *   extraction: it concentrates authority in credentialed disciplines,
 *   excludes alternative epistemologies and experiential knowledge systems,
 *   and extracts value from research subjects and marginalized communities
 *   whose lived experience becomes data without reciprocal authority. The
 *   measurement series tracks an extraction accumulation pattern: base
 *   extractiveness rises from 0.52 to 0.68 across the interval while
 *   suppression hardens from 0.54 to 0.71. Theater ratio climbs from 0.28 to
 *   0.42, indicating rising performative activity (diversity initiatives,
 *   acknowledgment of alternative epistemologies) alongside enforcement of
 *   the core gatekeeping logic. The coercion grid shows organizational-level
 *   suppression (0.62 to 0.68) and accessibility collapse (0.81 to 0.85)
 *   rising more steeply than structural or individual levels, indicating that
 *   the constraint operates through institutional machinery that targets
 *   organized challengers more heavily than atomized individuals.
 *
 * KEY AGENTS:
 *   - Credentialed academic disciplines: institutional agenda-setter, controls methodology standards and gatekeeping
 *   - Peer review institutions: institutional agenda-setter and beneficiary, operates the certification apparatus
 *   - Research funding bodies: institutional beneficiary and partial agenda-setter, directs capital flows to credentialed research
 *   - Experiential knowledge holders: powerless payers, excluded from legitimacy via lack of credentialing
 *   - Marginalized communities: moderate payers with identity-locked exit, subject to double devaluation (outsider status + epistemic devaluation)
 *   - Alternative epistemologies: organized but excluded, held in subordinated position
 *   - Research subjects: powerless payers and excluded, provide material for knowledge extraction without voice in validation
 *   - Interdisciplinary practitioners: moderate payers with constrained exit, face friction for crossing discipline boundaries
 *   - Methodological dissidents: credentialed moderates with constrained exit, face barriers for questioning methodology canon
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.71).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Gatekeeping (Legitimate Knowledge Boundary)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science_and_technology_studies").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'bc675786-b692-4910-92b0-7b8ef14337d9').
narrative_ontology:cs_kernel_codification('bc675786-b692-4910-92b0-7b8ef14337d9', fixed_text).
narrative_ontology:cs_authority_grounding('bc675786-b692-4910-92b0-7b8ef14337d9', lineage).
narrative_ontology:cs_interpretation_layer_present('bc675786-b692-4910-92b0-7b8ef14337d9').
narrative_ontology:cs_reading_relation('bc675786-b692-4910-92b0-7b8ef14337d9', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc675786-b692-4910-92b0-7b8ef14337d9', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('bc675786-b692-4910-92b0-7b8ef14337d9', foundational, methodological_rigor_truth_proxy).
narrative_ontology:cs_axiom_status(methodological_rigor_truth_proxy, holdable).
narrative_ontology:cs_axiom_grounding('bc675786-b692-4910-92b0-7b8ef14337d9', methodological_rigor_truth_proxy, empirically_contingent).
narrative_ontology:cs_axiom('bc675786-b692-4910-92b0-7b8ef14337d9', foundational, credentialed_gatekeeper_necessity).
narrative_ontology:cs_axiom_status(credentialed_gatekeeper_necessity, holdable).
narrative_ontology:cs_axiom_grounding('bc675786-b692-4910-92b0-7b8ef14337d9', credentialed_gatekeeper_necessity, instrumental).
narrative_ontology:cs_reference_frame('bc675786-b692-4910-92b0-7b8ef14337d9', methodological_rigor_as_epistemic_standard).
narrative_ontology:cs_drift_state('bc675786-b692-4910-92b0-7b8ef14337d9', contemporary_pluralism_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc675786-b692-4910-92b0-7b8ef14337d9', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_disciplines).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_bodies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, alternative_epistemologies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_practitioners).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialing_bureaucracies).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_gatekeepers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, research_subjects).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_practitioners).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_rigor_as_truth_proxy).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, expert_consensus_as_legitimacy_marker).
narrative_ontology:constraint_vindicates(legitimate_knowledge_boundary__credentialed_expertise_reading, disciplinary_boundary_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces standards for what counts as legitimate knowledge through peer review gatekeeping, methodology curricula, and credentialing mechanisms. Controls journal access, conference platforms, and funding allocation. Defends the boundary by requiring credentials and methodological compliance for entry. Derives institutional authority and resource flows from gatekeeping function.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_disciplines, agenda_setter,
    institutional, generational, arbitrage, global).

% Operate the validation apparatus (journals, conferences, review panels) that certifies legitimate knowledge. Benefit from the legitimacy premium attached to their stamp and the labor of unpaid reviewers. Actively enforce methodology standards and gatekeeping logic through rejection and revision demands.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_institutions, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_institutions, beneficiary).

% Allocate research funding primarily to credentialed researchers and institutions pursuing methodologically rigorous inquiry as defined by the constraint. Direct capital flows to disciplines that comply with the boundary. Use the credential and peer-review system to justify funding decisions and validate research outputs.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_bodies, agenda_setter).

% Possess knowledge derived from lived experience — environmental familiarity, occupational skill, community inheritance — but lack credentialing in the methodological framework. Pay the cost of exclusion through inability to access funding, publication platforms, or institutional legitimacy. Their knowledge is classified as anecdotal, subjective, or non-generalizable when it conflicts with credentialed findings.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders, payer,
    powerless, biographical, trapped, local).

% Hold knowledge about their own conditions, needs, and effective interventions. Face barriers to credentialing rooted in prior exclusion (lack of access to doctoral pathways, cultural dissonance with methodological frameworks, language barriers). Their knowledge about themselves is systematically devalued when it diverges from outsider-credentialed research. Identity fusion with community knowledge makes exit from the epistemic arrangement costly beyond the instrumental barrier.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, marginalized_communities, payer,
    moderate, biographical, identity_locked, regional).

% Indigenous knowledge systems, feminist standpoint epistemology, decolonial frameworks, and participatory research methodologies exist but are structurally excluded from the legitimate-knowledge category. They are taught as cases, critiques, or supplementary perspectives, but not as generators of legitimate primary knowledge. The constraint's enforcement keeps these epistemologies in a subordinated position even when they produce verifiable insights.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, alternative_epistemologies, excluded,
    organized, generational, trapped, global).

% Provide the material (data, behaviors, lived experience) from which credentialed researchers extract legitimate knowledge. Typically do not control the questions asked, the framing of findings, or the use of the knowledge generated. Face extraction of their experience without reciprocal benefit or voice in validation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, research_subjects, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, research_subjects, excluded).

% Hold credentials in one discipline but work across boundaries (design, policy, practice). Face pressure to publish in discipline-specific venues and follow methodology standards set by the home discipline, even when interdisciplinary work requires different validation logic. Can access some resources but with friction and devaluation of collaborative or applied work.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_practitioners, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, interdisciplinary_practitioners, beneficiary).

% Universities, professional licensing bodies, and accreditation agencies administer the credentialing apparatus. Benefit from the scarcity premium the constraint creates (the prestige value of degrees is maintained by limiting who gets them). Enforce methodology standards in curricula and licensing exams.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialing_bureaucracies, beneficiary,
    institutional, generational, mobile, national).

% Credentialed researchers who question the constraint's equation of methodological rigor with legitimate knowledge — those who argue for mixed methods, participatory design, or value integration in research. Face barriers to publication, grant funding, and institutional advancement when they deviate from the discipline's methodology canon. Often remain partially inside but with diminished authority.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_dissidents, excluded,
    moderate, biographical, constrained, global).

% Government agencies, foundations, and private sponsors that allocate research funding. Rely on the credential and peer-review system to allocate capital at scale and distance themselves from direct responsibility for research directions. Benefit from the legitimacy conferred by the constraint (funding decisions appear evidence-based because they use credentialed validators).
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_gatekeepers, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_gatekeepers, beneficiary).

% Science and technology studies scholars, policy analysts, and institutional auditors who examine how the knowledge legitimation system operates. Can document gatekeeping patterns, measure who is excluded, and track the constraint's effects without direct material stake in its preservation or dissolution.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, observer_seat, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_disciplines).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates knowledge validation across dispersed practitioners into a unified certification standard: researchers can trust findings published in peer-reviewed venues without independently verifying all prior work; institutions can allocate resources to credentialed researchers without individual evaluation; society can identify genuine knowledge from noise without expertise itself.
% TRANSFER_FUNCTION: Extracts authority, funding, and institutional position from researchers and communities whose knowledge is devalued as non-rigorous or subjective, to credentialed academic disciplines and peer-review gatekeepers who control the certification apparatus. Moves research subjects' lived experience into the knowledge commons as data, without reciprocal stake-holding in the findings.
% ABSENT_VOICES: Indigenous knowledge systems, community-based participatory researchers, experiential experts, and alternative epistemologies are structurally excluded from the conversation that defines what counts as legitimate. If present, they would argue for epistemological pluralism, co-validation, and power-sharing in what gets certified as knowledge. Their frameworks for validation—consensus among knowledge-holders, intergenerational testing, pragmatic fit to community needs—are treated as supplements to rigor, not as legitimate alternatives.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, funding would flow to a wider range of validation frameworks; alternative epistemologies would have access to the platforms and resources currently closed to them; the legitimacy premium attached to credentialing would erode; research agenda-setting would include more voices from affected communities. The institutional power of universities and discipline-specific peer review would diminish substantially. Knowledge production would decentralize and diversify.
% FOUNDING_PROBLEM: Early modern natural philosophy faced a crisis of authority: competing claims about nature's laws, no reliable way to distinguish genuine discovery from speculation or fraud. The methodological rigor standard emerged as a solution: systematic observation, reproducibility, mathematical proof, and peer scrutiny as mechanisms to sort true from false claims. The founding problem was epistemic chaos — how to know which knowledge claims were reliable.
% FOUNDING_PROBLEM_CORROBORATION: The academic establishment and credentialing institutions attest the founding problem remains live, citing ongoing threats of pseudoscience and misinformation. Alternative epistemology practitioners and marginalized communities attest the founding problem is substantially solved within their own validation systems and the constraint now persists as a way to maintain disciplinary power and exclude competing frameworks. Historical analysis of the rise of the scientific method and replicability studies showing disciplinary gatekeeping effects support the shifted-function reading; testimony from indigenous scholars and participatory researchers documents effective knowledge validation outside the credentialed framework.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint concentrates authority in institutional disciplines without proportional return to those whose knowledge is devalued. Suppression is higher (0.71) because the boundary is actively enforced through rejection mechanisms, credentialing gatekeeping, and resource allocation. Accessibility collapse is high (0.78) because alternatives to credentialed methodology are presented as not-real-knowledge, closing off epistemic exit routes. The measurement series show extraction accumulating over time: as alternative epistemologies become more visible and articulate (resistance rising from 0.31 to 0.35 at individual level), the constraint intensifies enforcement (suppression rising from 0.54 to 0.71 at structural level) and increases theater (diversity initiatives, acknowledgment language) while maintaining core gatekeeping. This is a classic institutional ratchet: the constraint absorbs critiques through performative concessions while hardening enforcement of the actual boundary. The coercion grid shows the constraint operates differently at different levels. At the organizational level (universities, departments, review boards), alternatives are nearly inaccessible (0.85 collapse) and suppression is heavy (0.68). At the individual level (a lone researcher with alternative epistemology), collapse is lower (0.72) because escape routes exist (publishing in non-peer-reviewed venues, working outside academia), but the cost is high (career, funding, legitimacy). At the class level (the community of alternative knowledge-holders collectively), both accessibility and resistance are moderate, indicating sustained contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (credentialed disciplines), the constraint is presented as methodological rigor protecting truth from fraud — a coordination function. From that seat, extraction is minimal (the constraint costs them resources they freely deploy) and suppression is justified as quality control. From the payer seat (experiential knowledge holders, marginalized communities), the same structure is gatekeeping that devalues their knowledge and reserves legitimacy for outsider-credentialed findings about their own conditions — asymmetric extraction. From the excluded seat (alternative epistemologies), it is institutional colonization that treats non-Western and non-academic knowledge frameworks as interesting supplements rather than legitimate generators of knowledge. The engine computes each seat's type from the structural data; the perspectival gap between beneficiary and payer seats is where mandatrophy should show (the constraint is claimed as rope/coordination but computes as tangled_rope or snare from payer seats due to the asymmetric extraction and active enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Credentialed academic disciplines are beneficiaries at the high end of directionality (d near 0.0): they set the rules, collect the authority premium, and have arbitrage-level exit (they can move between disciplines while staying inside the credentialed system). Peer review institutions benefit similarly (d near 0.1): they operate the certification machinery and gain legitimacy from gatekeeping. Experiential knowledge holders are targets at the high end (d near 1.0): they bear the cost of exclusion (devaluation, unfunded research, inaccessibility to platforms), have trapped exit (cannot gain credentials in their own knowledge domain), and pay through inability to influence what gets certified. Marginalized communities are targets (d near 0.9): they have moderate power through organization and advocacy, but identity-locked exit means leaving the constraint means abandoning community identity or community stake-holding in knowledge about themselves. Research subjects are targets (d near 1.0): powerless, trapped, their material extracted without reciprocal benefit. The engine derives directionality automatically from the beneficiary/victim declarations and exit options; no override needed here — the structural asymmetry is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is present and should be detected: the constraint was founded to solve the problem of distinguishing reliable knowledge from speculation (founding_problem_status = live in the agenda-setter account, contested overall). But the founding problem is substantially solved within alternative frameworks (marginalized communities have effective knowledge validation systems; indigenous epistemologies produce reliable, tested knowledge over generational timescales; participatory research produces verifiable, actionable findings). The constraint persists not because the founding problem is live but because institutional actors (credentialed disciplines, funding bodies, credentialing bureaucracies) benefit from maintaining scarcity and gatekeeping. This is the mandatrophy signature: the original coordination function (validating reliable knowledge) has been hollowed out and replaced by extraction (concentrating authority). The measured divergence between claimed_type (tangled_rope — the beneficiary reading, which emphasizes coordination + asymmetric extraction) and the likely computed types (snare from payer and excluded seats, due to high extraction + high suppression + identity-locked/trapped exit) is the second mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem of unreliable knowledge claims actually solved within the credentialed framework, or does the constraint persist primarily to maintain institutional authority?',
    'Comparative epistemology: audit knowledge validation outcomes across credentialed and non-credentialed systems (does peer review prevent pseudoscience, or does it just concentrate it within credentialed prestige?). Study replicability and falsification rates in published research. Document effective knowledge production in alternative frameworks (indigenous environmental management, participatory community health).',
    'If the founding problem is substantially solved outside credentialed systems, the constraint is mandatrophy — extraction riding on a solved coordination function. Reclassification to snare/piton would follow. If the founding problem is still live primarily within credentialed research (cutting-edge fields still generate unreliable claims), the extraction reading weakens and coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether methodological rigor in credentialed systems provides epistemic advantage over alternative validation systems.').

omega_variable(
    alternative_epistemology_foreclosure,
    'Does this reading structurally foreclose the experiential_pluralism and hybrid_coproduction readings, or can they coexist as competing claims about legitimate knowledge?',
    'Examine institutional practices: are alternative epistemologies structurally excluded (cannot enter the knowledge commons even if epistemically rigorous), or merely devalued (can enter but with lower prestige)? Track policy changes: if institutions adopt co-production or pluralism, do they displace or supplement credentialed gatekeeping?',
    'If foreclosure is genuine (credentialed methodology logically requires the exclusion of other frameworks), the relation is forecloses. If coexistence is possible (different parties hold different readings while both remain live), the relation is coexists_with. If this reading creates structural pressure on the siblings but does not eliminate them, the relation is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_epistemology_foreclosure, conceptual, 'The logical structure of the relation between credentialed expertise gatekeeping and epistemological pluralism.').

omega_variable(
    identity_lock_in_marginalized_knowledge,
    'For marginalized communities and indigenous knowledge practitioners, is the barrier to credentialing primarily instrumental (access to doctoral pathways) or primarily internalized (fusion of identity with non-credentialed knowledge sources that makes pursuing credentials feel like epistemic betrayal)?',
    'Longitudinal study of credentialing pathways for practitioners from marginalized communities: do they cite institutional barriers (cost, curriculum fit, geographic access) or identity/cultural friction (conflict between academic training and community knowledge sources, felt colonization)? Post-exit trajectory: do credentialed practitioners from marginalized communities maintain ties to alternative frameworks, or does credentialing complete the identity reorientation?',
    'If primarily instrumental, the constraint''s suppression is structural and removable by access policies. If primarily internalized, the identity-lock exit remains even after instrumental barriers fall, and the constraint is more deeply extractive (targets internal self-perception). The measured suppression (0.71) is likely a blend; decomposing it clarifies remediation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_marginalized_knowledge, empirical, 'Mechanism of identity-lock exit for marginalized epistemology practitioners.').

omega_variable(
    methodology_asymmetry_across_disciplines,
    'Is methodological rigor enforced symmetrically across all academic disciplines, or do privileged disciplines (physics, molecular biology) get to define rigor while others (humanities, indigenous studies) face asymmetric scrutiny?',
    'Bibliometric analysis: rejection rates, replication requirements, and evidence standards for publications across disciplines. Compare methodological requirements for funding approval (does a quantitative field require the same rigor standard as a qualitative one?). Track gatekeeping narratives: when do journals invoke methodology vs. disciplinary norm?',
    'If enforcement is asymmetric (high-status disciplines define rigor and lower-status ones must exceed it), the constraint is more extractive than if enforcement is uniform — it becomes a tool for disciplinary hierarchy, not just knowledge validation. Asymmetric enforcement would strengthen the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_asymmetry_across_disciplines, empirical, 'Whether methodology standards are enforced uniformly across disciplines or used as a tool for disciplinary gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(legi_tr_t8, observed).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(legi_tr_t16, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t36, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 36, 0.41).
narrative_ontology:measurement_basis(legi_tr_t36, observed).
narrative_ontology:measurement(legi_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(legi_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(legi_be_t8, observed).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(legi_be_t16, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t36, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(legi_be_t36, observed).
narrative_ontology:measurement(legi_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(legi_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(legi_su_t8, observed).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement_basis(legi_su_t16, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t36, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(legi_su_t36, observed).
narrative_ontology:measurement(legi_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(legi_su_t50, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(legi_grid_01, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(class), 0, 0.76).
narrative_ontology:measurement(legi_grid_02, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(class), 50, 0.78).
narrative_ontology:measurement(legi_grid_03, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(legi_grid_04, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(individual), 50, 0.72).
narrative_ontology:measurement(legi_grid_05, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(organizational), 0, 0.81).
narrative_ontology:measurement(legi_grid_06, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(organizational), 50, 0.85).
narrative_ontology:measurement(legi_grid_07, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(structural), 0, 0.74).
narrative_ontology:measurement(legi_grid_08, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(structural), 50, 0.82).
narrative_ontology:measurement(legi_grid_09, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(class), 0, 0.48).
narrative_ontology:measurement(legi_grid_10, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(class), 50, 0.54).
narrative_ontology:measurement(legi_grid_11, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(individual), 0, 0.31).
narrative_ontology:measurement(legi_grid_12, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(individual), 50, 0.35).
narrative_ontology:measurement(legi_grid_13, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(organizational), 0, 0.52).
narrative_ontology:measurement(legi_grid_14, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(organizational), 50, 0.58).
narrative_ontology:measurement(legi_grid_15, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(structural), 0, 0.41).
narrative_ontology:measurement(legi_grid_16, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(structural), 50, 0.44).
narrative_ontology:measurement(legi_grid_17, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_18, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(class), 50, 0.62).
narrative_ontology:measurement(legi_grid_19, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(legi_grid_20, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(individual), 50, 0.48).
narrative_ontology:measurement(legi_grid_21, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(legi_grid_22, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(organizational), 50, 0.74).
narrative_ontology:measurement(legi_grid_23, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(legi_grid_24, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(structural), 50, 0.71).
narrative_ontology:measurement(legi_grid_25, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_26, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(class), 50, 0.64).
narrative_ontology:measurement(legi_grid_27, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(legi_grid_28, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(individual), 50, 0.46).
narrative_ontology:measurement(legi_grid_29, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(organizational), 0, 0.62).
narrative_ontology:measurement(legi_grid_30, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(organizational), 50, 0.68).
narrative_ontology:measurement(legi_grid_31, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(legi_grid_32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(structural), 50, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.14).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_credentialing_gatekeeping).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, research_funding_allocation_bias).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, indigenous_knowledge_devaluation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the legitimate_knowledge_boundary kernel. Three competing readings exist: credentialed_expertise_reading (this story) treats legitimacy as deriving from methodological rigor and peer review, establishing institutional gatekeeping as the core extraction mechanism. Experiential_pluralism_reading argues legitimacy arises from lived experience and community validation, treating credentialed gatekeeping as ideological colonization. Hybrid_coproduction_reading seeks integration of both, treating the credentialed constraint as asymmetrically capturing the knowledge commons. Each reading has the same referent (the standing arrangement of credentialed epistemology) but different ε values. This story's ε=0.68 reflects the reading's assessment of the arrangement as substantially extractive; a plural reading might author lower ε (seeing more genuine coordination), an alternative reading might author higher ε (seeing pure extraction without coordination). The three stories are linked by network.affects_constraints showing family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
