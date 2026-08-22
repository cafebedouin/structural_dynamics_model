% ============================================================================
% CONSTRAINT STORY: ai_human_relationship__instrumental_subsidiarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_human_relationship__instrumental_subsidiarity, []).

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
 *   constraint_id: ai_human_relationship__instrumental_subsidiarity
 *   human_readable: AI as Neutral Tool: Instrumental Subsidiarity Reading
 *   domain: political_theology/technology_ethics/governance
 *
 * SUMMARY:
 *   The instrumental-subsidiarity reading treats AI as a neutral tool whose
 *   impact depends on governance choices, regulatory frameworks, and ethical
 *   use-case implementation. It instantiates one side of a contested
 *   theological and philosophical boundary: whether technology carries
 *   inherent moral properties or is fundamentally responsive to human
 *   direction through law and ethics. This reading is one of three
 *   interpretations of the AI-human-relationship kernel (alongside
 *   incarnational_humanism and technocratic_optimization). It benefits
 *   legal/regulatory institutions and corporate developers by locating
 *   responsibility in governance rather than design; it extracts from
 *   marginalized communities, displaced workers, and algorithmic subjects by
 *   classifying their harms as use-case problems rather than technological
 *   properties. The constraint is claimed as tangled_rope because it solves
 *   genuine coordination (how to govern technology responsibly) while
 *   asymmetrically extracting (responsibility moves away from designers). The
 *   measurement series tracks extractiveness plateauing as the regulatory
 *   framework matures, theater ratio rising as impact-assessment compliance
 *   becomes performative, and suppression-requirement rising as excluded
 *   voices must be actively kept from the policy table.
 *
 * KEY AGENTS:
 *   - legal_regulatory_apparatus: Agenda-setter; institutional power; administers neutrality doctrine and transparency requirements
 *   - corporate_technology_developers: Beneficiary; institutional power; benefits from responsibility displacement to governance
 *   - displaced_workers: Payer; powerless, trapped; bear automation costs classified as policy failures
 *   - marginalized_communities: Payer; powerless, identity-locked; subject to algorithmic systems whose harms are classified as governance problems
 *   - transparency_advocates: Beneficiary; organized power; mobilize neutrality premise to argue for stronger oversight
 *   - alignment_researchers: Excluded; organized power; claim AI carries non-neutral structural properties, kept from policy conversation
 *   - catholic_episcopal_teaching_authority: Observer; institutional power; issues moral guidance critiquing all three readings
 *   - algorithmic_subjects: Payer; powerless, trapped, universal scope; classified by systems they cannot refuse or contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, 0.62).
domain_priors:suppression_score(ai_human_relationship__instrumental_subsidiarity, 0.58).
domain_priors:theater_ratio(ai_human_relationship__instrumental_subsidiarity, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, extractiveness, 0.62).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(ai_human_relationship__instrumental_subsidiarity, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_human_relationship__instrumental_subsidiarity, tangled_rope).
narrative_ontology:human_readable(ai_human_relationship__instrumental_subsidiarity, "AI as Neutral Tool: Instrumental Subsidiarity Reading").
narrative_ontology:topic_domain(ai_human_relationship__instrumental_subsidiarity, "political_theology/technology_ethics/governance").

domain_priors:requires_active_enforcement(ai_human_relationship__instrumental_subsidiarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ai_human_relationship__instrumental_subsidiarity, '16e662ec-2420-41b4-9660-72cef7a92c10').
narrative_ontology:cs_kernel_codification('16e662ec-2420-41b4-9660-72cef7a92c10', fixed_text).
narrative_ontology:cs_authority_grounding('16e662ec-2420-41b4-9660-72cef7a92c10', extraction).
narrative_ontology:cs_interpretation_layer_present('16e662ec-2420-41b4-9660-72cef7a92c10').
narrative_ontology:cs_reading_relation('16e662ec-2420-41b4-9660-72cef7a92c10', ai_human_relationship__incarnational_humanism, coexists_with).
narrative_ontology:cs_reading_relation('16e662ec-2420-41b4-9660-72cef7a92c10', ai_human_relationship__technocratic_optimization, coexists_with).
narrative_ontology:cs_axiom('16e662ec-2420-41b4-9660-72cef7a92c10', foundational, technology_is_morally_neutral).
narrative_ontology:cs_axiom_status(technology_is_morally_neutral, holdable).
narrative_ontology:cs_axiom_grounding('16e662ec-2420-41b4-9660-72cef7a92c10', technology_is_morally_neutral, instrumental).
narrative_ontology:cs_axiom('16e662ec-2420-41b4-9660-72cef7a92c10', foundational, responsibility_determines_harm_not_design).
narrative_ontology:cs_axiom_status(responsibility_determines_harm_not_design, holdable).
narrative_ontology:cs_axiom_grounding('16e662ec-2420-41b4-9660-72cef7a92c10', responsibility_determines_harm_not_design, conventional).
narrative_ontology:cs_axiom('16e662ec-2420-41b4-9660-72cef7a92c10', secondary, subsidiarity_delegates_to_governance_not_redesign).
narrative_ontology:cs_axiom_status(subsidiarity_delegates_to_governance_not_redesign, holdable).
narrative_ontology:cs_axiom_grounding('16e662ec-2420-41b4-9660-72cef7a92c10', subsidiarity_delegates_to_governance_not_redesign, conventional).
narrative_ontology:cs_reference_frame('16e662ec-2420-41b4-9660-72cef7a92c10', technology_instrumentally_responsive_to_governance).
narrative_ontology:cs_drift_state('16e662ec-2420-41b4-9660-72cef7a92c10', post_algorithmic_discrimination_documented, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('16e662ec-2420-41b4-9660-72cef7a92c10', '').
narrative_ontology:cs_kernel_id(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, legal_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, corporate_technology_developers).
narrative_ontology:constraint_beneficiary(ai_human_relationship__instrumental_subsidiarity, transparency_advocates).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, displaced_workers).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, marginalized_communities).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, algorithmic_subjects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ai_human_relationship__instrumental_subsidiarity, corporate_technology_developers).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, technological_neutrality_doctrine).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, subsidiarity_principle).
narrative_ontology:constraint_vindicates(ai_human_relationship__instrumental_subsidiarity, human_dignity_protection_via_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the regulatory framework claiming AI is a neutral tool whose impact depends on use-case governance. Develops transparency requirements, impact assessments, and ethical guidelines that presume the technology itself carries no inherent directionality. Administers compliance regimes that treat harms as policy failures, not technological properties.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, legal_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Benefits from the neutrality framing: it relocates responsibility for harms from technology design to regulatory oversight and end-user implementation. They pay compliance costs (auditing, transparency infrastructure) but retain design authority and can arbitrage between regulatory jurisdictions. The framing preserves their capacity to claim 'our tool is neutral; how you use it is your choice.'
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, corporate_technology_developers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ai_human_relationship__instrumental_subsidiarity, corporate_technology_developers, payer).

% Experience automation as elimination of their labor market position. Under the instrumental-subsidiarity reading, their displacement is classified as a use-case problem (labor policy should have managed transition), not a feature of the technology. They bear the cost while the technology's neutrality is maintained as doctrine. Their exit options are constrained by the pace of labor-market transition and geographic immobility.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, displaced_workers, payer,
    powerless, biographical, trapped, national).

% Subject to algorithmic systems trained on historical data that encode existing discrimination (hiring, lending, criminal justice, welfare eligibility). The instrumental reading locates the problem in training data and regulatory policy, not in the algorithmic method itself. They cannot exit the systems applied to them and carry identity-locked constraints (their classification by the system becomes documentation used against them).
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, marginalized_communities, payer,
    powerless, biographical, identity_locked, national).

% Benefit from the instrumental-subsidiarity framing because it validates their core advocacy: if AI is neutral, then transparency, ethical guidelines, impact assessment, and regulatory oversight are the solutions. They mobilize the neutrality premise to argue for stronger governance frameworks and corporate accountability mechanisms. Their institutional position and network exit (they can relocate advocacy effort) gives them mobility.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, transparency_advocates, beneficiary,
    organized, biographical, mobile, global).

% Argue that AI systems carry structural properties (goal misalignment, instrumental convergence, value lock-in) that operate independently of use-case governance and are not resolvable by regulation alone. They are excluded from the primary policy conversation because their premise contradicts the neutrality doctrine. Their research is treated as technical detail rather than fundamental to the subsidiarity framing.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, alignment_researchers, excluded,
    organized, generational, constrained, global).

% Issues normative guidance on technology ethics from the standpoint of integral human development, common good, and solidarity. Takes an analytical position from which to critique all three readings (incarnational, instrumental, technocratic) and their anthropological assumptions. Does not directly regulate but shapes the moral vocabulary in which technology debates occur.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, catholic_episcopal_teaching_authority, observer,
    institutional, civilizational, analytical, global).

% People classified, scored, and sorted by algorithmic systems without meaningful consent or recourse. Under the instrumental reading, their harm is a governance failure (regulators should have required consent/transparency mechanisms). They carry the cost of being continuously classified by systems they do not control and cannot refuse.
narrative_ontology:constraint_stakeholder(ai_human_relationship__instrumental_subsidiarity, algorithmic_subjects, payer,
    powerless, immediate, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ai_human_relationship__instrumental_subsidiarity, legal_regulatory_apparatus).
narrative_ontology:fixing_cost_class(ai_human_relationship__instrumental_subsidiarity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates technology development, deployment, and use around the normative claim that AI is a neutral tool whose impact is determined by governance choices and use-case implementation. Solves the problem of how to permit rapid technological innovation while addressing public concern about harms — the solution is to treat harms as regulatory/policy problems, not as inherent to the technology.
% TRANSFER_FUNCTION: Moves responsibility for algorithmic harms from technology designers (who claim neutrality) to legal/regulatory institutions (who must govern use) and affected parties (who must adapt). Moves the burden of establishing harm from technology providers to regulators and victims. Transfers authority over technology ethics from engineering communities and affected populations to legal and policy experts.
% ABSENT_VOICES: Displaced workers, marginalized communities experiencing algorithmic discrimination, and researchers who claim AI systems carry non-neutral structural properties are structurally excluded from the primary policy conversation. They would testify that the technology itself embeds directionality (automation selects for capital efficiency, optimization metrics encode existing power structures) that regulation alone cannot remedy. Alternative technology readings that place harms at the system level rather than the use-case level are kept out of the legitimacy frame.
% DISAPPEARANCE_RATIONALE: If the instrumental-subsidiarity reading and its regulatory apparatus vanished, technology development would reorganize: without the neutrality premise shielding designers from responsibility claims, engineering culture would face direct accountability for algorithmic harms. Regulatory burden would shift earlier in the development cycle. Marginalized communities and workers might claim standing to contest deployment rather than seek remedies after harm. The absent voices would enter the conversation with structural authority.
% FOUNDING_PROBLEM: Early AI deployment generated harms (bias in automated hiring, discriminatory lending systems, job displacement, privacy violations) that neither the technology community nor existing regulatory frameworks adequately addressed. The instrumental-subsidiarity reading was developed to solve the problem: technology is neutral; harms reflect policy/governance failures; regulation and ethical guidelines can decouple technology development from direct responsibility for harms.
% FOUNDING_PROBLEM_CORROBORATION: Regulatory agencies and technology corporations attest the founding problem remains live and is managed by governance frameworks. Displaced workers, marginalized communities, alignment researchers, and social-justice technologists attest the founding problem persists because the neutrality premise misdiagnoses it — they argue the problem is not governance lag but structural properties of the technology that regulation does not address. Independent research (AI audits, labor-displacement studies, algorithmic-bias documentation) from outside the regulatory/corporate beneficiary set supports the contested reading.
narrative_ontology:disappearance_verdict(ai_human_relationship__instrumental_subsidiarity, world_rearranges).
narrative_ontology:founding_problem_status(ai_human_relationship__instrumental_subsidiarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ai_human_relationship__instrumental_subsidiarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ai_human_relationship__instrumental_subsidiarity, 'none', 1).
narrative_ontology:epsilon_provenance(ai_human_relationship__instrumental_subsidiarity, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_human_relationship__instrumental_subsidiarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_human_relationship__instrumental_subsidiarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_human_relationship__instrumental_subsidiarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.62) and rising because the neutrality premise systematically relocates responsibility for harms away from technology designers toward regulatory apparatus and affected populations, concentrating authority with those who set the interpretation. Suppression is high (0.58) because alignment researchers, structural-property arguments, and incarnational-humanism perspectives must be actively excluded for the neutrality doctrine to hold — the regulatory framework explicitly rejects non-neutral readings of AI as outside the proper scope of governance (framed as technical, philosophical, or ideological rather than policy-relevant). Theater rises over the interval (0.28→0.41) because transparency requirements, impact assessments, and ethics boards become increasingly performative: they appear to address harm while the neutrality premise prevents structural changes to technology. The measurement grid is shared across all three metrics at each time point (t=0,5,10,15,20,25). Extractiveness plateaus (0.59→0.62) as the regulatory regime stabilizes and reaches its steady-state extraction level; further gains would require new doctrinal moves (e.g., explicit harm-transfer mechanisms). Suppression also plateaus: active exclusion of non-neutral readings reaches equilibrium as the policy frame solidifies.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (regulatory apparatus + corporate developers) experiences the constraint as coordination + minor compliance overhead. The victim seat (displaced workers, marginalized communities) experiences the same constraint as pure extraction with no real governance remedy because the neutrality premise prevents structural change. The excluded seat (alignment researchers) experiences suppression: their intellectual contribution is treated as outside the legitimate policy domain. The observer seat (catholic teaching authority) experiences the constraint as a theological proposition that requires refutation on incarnational and solidarity grounds.
 *
 * DIRECTIONALITY LOGIC:
 *   The regulatory apparatus and corporate developers benefit from the neutrality premise and have arbitrage-grade exit (they can shift the constraint's frame or relocate jurisdiction). Displaced workers are trapped and identity-locked (their labor-market position depends on the pace of technology deployment). Marginalized communities are identity-locked (they are continuously classified by systems they do not control). Alignment researchers are constrained and suppressed (they cannot participate in policy without accepting the neutrality frame). Algorithmic subjects are trapped and identity-locked (universal scope means the systems apply everywhere; identity-locked because their algorithmic classification becomes documentation used against them). The directionality derivation flows directly from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (harms from early AI deployment) is contested in its diagnosis: beneficiary parties claim it is a governance lag (policy has not caught up to technology), while victim parties and excluded researchers claim it is a structural property of the technology that governance cannot remedy. The instrumental-subsidiarity reading resolves this by fiat: it declares the technology neutral and therefore governance is the solution. This is mandatrophy-adjacent because the reading prevents recognition that the founding problem might persist precisely because the neutrality premise prevents structural intervention. If displaced workers and marginalized communities continue to experience algorithmic harms despite transparency requirements and regulatory oversight, the constraint's founding problem is dead (the problem it was supposed to solve is not being solved) while the constraint persists (regulation and ethics frameworks continue to treat AI as neutral). This is exactly the mandatrophy condition: the constraint has outlived the function it was supposed to serve, but it persists because it benefits the agenda-setters. The measurement plateau (extractiveness and suppression both flatline after t=15) suggests the constraint has reached its mature, inertial state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_neutrality_premise,
    'Is AI genuinely neutral—a tool whose impact is determined entirely by use-case governance and ethical frameworks—or does the technology itself embed non-neutral properties (optimization biases, automation selection for capital efficiency, encoding of existing power structures in training data)?',
    'Comparative analysis of AI systems deployed identically but in different governance contexts with different ethical frameworks: if harms persist despite governance variation, technological neutrality is refuted. Analysis of what AI systems optimize for absent explicit goal specification: if systems show consistent patterns (capital efficiency, labor displacement, existing-power-structure preservation) independent of governance intent, neutrality is refuted.',
    'If technology proves non-neutral, the constraint reclassifies from tangled_rope (coordination + asymmetric extraction) to snare (pure extraction). Responsibility would shift from governance to design. Remedy would require changing the technology, not regulating use-cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_neutrality_premise, empirical, 'Whether AI carries intrinsic directional properties or is truly response to governance.').

omega_variable(
    governance_remedy_sufficiency,
    'Can transparency requirements, impact assessments, ethical guidelines, and regulatory oversight actually remediate the harms attributed to AI, or do these governance mechanisms merely displace responsibility while harms persist?',
    'Time-series analysis of algorithmic harm across jurisdictions with varying governance stringency: if harm rates decline with governance rigor, the reading is validated; if harms persist or shift form despite governance, the reading''s remedy-sufficiency is refuted. Post-intervention audits of regulated systems: do transparency + impact assessment prevent discriminatory outcomes?',
    'If governance proves insufficient to remedy harms, the constraint transitions from coordination (solving deployment responsibility) to performance (appearing to solve harms while they persist). The theater ratio would rise further and the constraint would show piton characteristics (inertial enforcement of procedures that have ceased to function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_remedy_sufficiency, empirical, 'Whether the subsidiarity-plus-regulation framework actually prevents algorithmic harms.').

omega_variable(
    substitution_of_structural_for_use_case_analysis,
    'Does the focus on governance frameworks systematically prevent recognition that harms are structural to the technology, thereby creating a category error that keeps policy solutions focused on use-case regulation rather than technology redesign?',
    'Discourse analysis and policy history: are proposals to change how AI systems are constructed (different optimization functions, different training-data sources, different architectural choices) treated as policy-relevant or dismissed as technical/philosophical? Do victim constituencies gain voice in design governance or only in post-hoc regulation?',
    'If the governance frame systematically excludes structural remedies, the constraint is not tangled_rope but snare: the neutrality premise and regulatory apparatus function to preserve technology design authority while appearing to address harms. The suppression measurement reflects active exclusion of alternative problem framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitution_of_structural_for_use_case_analysis, conceptual, 'Whether the instrumental reading prevents structural analysis of AI properties.').

omega_variable(
    incarnational_humanism_contradiction,
    'Does the instrumental-subsidiarity reading''s treatment of technology as neutral contradict Catholic Social Teaching''s incarnational anthropology, which holds that the human person is irreducible to optimization and that technology must be ordered to integral human development?',
    'Theological analysis of whether the neutrality premise can coexist with incarnational theology within Catholic teaching. If the incarnational reading is authoritative, does it foreclose the instrumental reading, or do they coexist in legitimate ecclesial tension?',
    'If incarnational theology forecloses instrumental neutrality, the constraint''s authority grounding within Catholic teaching is refuted and its legitimacy within that tradition collapses. The reading would survive only in secular governance contexts, and the network of constraint stories would show institutional divergence (Catholic and secular technology ethics readings as distinct constraints).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incarnational_humanism_contradiction, conceptual, 'Whether instrumental-subsidiarity coexists with or is foreclosed by incarnational anthropology.').

omega_variable(
    responsibility_relocation_mechanism,
    'The neutrality doctrine systematically relocates responsibility from technology designers to regulatory institutions and affected populations. Is this relocation a valid distribution of burden (responsibility follows authority to act) or an extraction mechanism (burden is placed on those without power to change the technology)?',
    'Analysis of who can change what: designers can change technology properties; regulators can change governance frameworks; affected populations can change... what? If affected populations have no change-authority, responsibility-relocation to them is extractive. If regulatory institutions can successfully mandate design changes, relocation may be valid.',
    'If relocation is extractive, suppression is inherent to the reading (active suppression of victim voice is required to maintain the doctrine). The constraint''s suppression score reflects this structural necessity, not just enforcement overhead.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responsibility_relocation_mechanism, conceptual, 'Whether responsibility-relocation is burden-distribution or extraction mechanism.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Which institutional actors genuinely hold the instrumental-subsidiarity reading as their primary commitment, and which hold it strategically to defend other interests (preservation of technology design authority, regulatory empire-building, capital-friendly governance)?',
    'Behavior analysis: do institutional actors holding the instrumental reading also support incarnational-humanism or technocratic-optimization readings when doing so would advance their interests? Do they shift readings contextually? The committer axis should reveal whether the reading is normatively held or strategically deployed.',
    'If the reading is held primarily for strategic reasons, the constraint is even more extractive than the measurement suggests: it combines asymmetric responsibility placement with false consensus about what the reading''s advocates actually believe. This would suggest the need for a sibling reading (strategic-instrumentalism) distinct from normative instrumentalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, empirical, 'Whether institutional actors genuinely commit to instrumental-subsidiarity or deploy it strategically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_human_relationship__instrumental_subsidiarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_h_tr_t0, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ai_h_tr_t0, projected).
narrative_ontology:measurement(ai_h_tr_t5, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(ai_h_tr_t5, projected).
narrative_ontology:measurement(ai_h_tr_t10, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(ai_h_tr_t10, projected).
narrative_ontology:measurement(ai_h_tr_t15, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(ai_h_tr_t15, observed).
narrative_ontology:measurement(ai_h_tr_t20, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(ai_h_tr_t20, observed).
narrative_ontology:measurement(ai_h_tr_t25, ai_human_relationship__instrumental_subsidiarity, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(ai_h_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ai_h_be_t0, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(ai_h_be_t0, projected).
narrative_ontology:measurement(ai_h_be_t5, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(ai_h_be_t5, projected).
narrative_ontology:measurement(ai_h_be_t10, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(ai_h_be_t10, projected).
narrative_ontology:measurement(ai_h_be_t15, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(ai_h_be_t15, observed).
narrative_ontology:measurement(ai_h_be_t20, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(ai_h_be_t20, observed).
narrative_ontology:measurement(ai_h_be_t25, ai_human_relationship__instrumental_subsidiarity, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(ai_h_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ai_h_su_t0, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(ai_h_su_t0, projected).
narrative_ontology:measurement(ai_h_su_t5, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(ai_h_su_t5, projected).
narrative_ontology:measurement(ai_h_su_t10, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(ai_h_su_t10, projected).
narrative_ontology:measurement(ai_h_su_t15, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 15, 0.57).
narrative_ontology:measurement_basis(ai_h_su_t15, observed).
narrative_ontology:measurement(ai_h_su_t20, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(ai_h_su_t20, observed).
narrative_ontology:measurement(ai_h_su_t25, ai_human_relationship__instrumental_subsidiarity, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(ai_h_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_human_relationship__instrumental_subsidiarity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ai_human_relationship__instrumental_subsidiarity, 0.18).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__incarnational_humanism).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, ai_human_relationship__technocratic_optimization).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, algorithmic_transparency_mandate__corporate_accountability).
narrative_ontology:affects_constraint(ai_human_relationship__instrumental_subsidiarity, labor_displacement__technological_inevitability).

% DUAL FORMULATION NOTE:
% This constraint is part of the ai_human_relationship kernel family (three readings: instrumental_subsidiarity, incarnational_humanism, technocratic_optimization). All three share a kernel text/doctrine (the role of technology in human flourishing) but offer different interpretations of what technology is, who is responsible for its effects, and what remedies are appropriate. The instrumental-subsidiarity reading is upstream in policy influence (it sets the regulatory framework that incarnational and technocratic readings then contest). The constraint also affects labor-displacement and algorithmic-transparency constraints, which inherit its neutrality premise and responsibility-relocation structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_human_relationship__instrumental_subsidiarity, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
