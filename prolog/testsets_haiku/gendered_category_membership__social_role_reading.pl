% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership via Social Performance and Recognition
 *   domain: social/political/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the social_role_reading of the
 *   gendered_category_membership kernel: membership in a gendered category is
 *   grounded in sustained social performance and recognition by others, not
 *   in biological markers or subjective identity alone. Trans women are
 *   conditionally included if they present as feminine and achieve social
 *   recognition; cis women are presumed members unless their performance
 *   deviates from norms; cis women who are gender-nonconforming and trans
 *   women who present atypically or are recently transitioned face systematic
 *   exclusion. The constraint is instantiated through distributed social
 *   gatekeeping—peers, colleagues, institutions collectively judge
 *   performance and grant or withhold recognition. The claimed type is
 *   tangled_rope: it solves a genuine coordination problem (how to signal and
 *   recognize gendered category membership in real-time social interaction
 *   without requiring biological data) while simultaneously extracting
 *   performance labor and conditionalizing recognition in ways that
 *   asymmetrically burden trans women and gender-nonconforming cis women. The
 *   author's claim and the metrics are intentionally independent: the
 *   constraint is claimed as tangled_rope while the authored extractiveness
 *   (0.52) and suppression (0.61) are moderate, reflecting that the
 *   coordination function is real but encumbered by extraction.
 *
 * KEY AGENTS:
 *   - trans_women_conditional_members: bear performance burden for conditional inclusion; identity-locked exit
 *   - cis_women_conforming_to_norms: presumed members; benefit from gatekeeping that excludes nonconforming others
 *   - cis_women_gender_nonconforming: face boundary challenges despite biological credentials; pay performance costs
 *   - social_enforcement_network: distributed agenda-setters who collectively judge performance and allocate recognition
 *   - trans_women_nonconforming_presentation: face systematic exclusion; trapped (cannot perform convincingly enough to gain recognition)
 *   - biological_essentialist_advocates: excluded voice; would reframe constraint as illegitimate overriding of biological facts
 *   - gender_identity_advocates: excluded voice; would reframe constraint as coercive gatekeeping against self-declaration
 *   - institutional_rule_makers: observers; their policy choices instantiate or resist this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.52).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.61).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership via Social Performance and Recognition").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social/political/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '60059e73-d84d-4a28-b049-d7da6d108ebc').
narrative_ontology:cs_kernel_codification('60059e73-d84d-4a28-b049-d7da6d108ebc', fixed_text).
narrative_ontology:cs_authority_grounding('60059e73-d84d-4a28-b049-d7da6d108ebc', distributed).
narrative_ontology:cs_reading_relation('60059e73-d84d-4a28-b049-d7da6d108ebc', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('60059e73-d84d-4a28-b049-d7da6d108ebc', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('60059e73-d84d-4a28-b049-d7da6d108ebc', foundational, social_performance_is_membership_ground).
narrative_ontology:cs_axiom_status(social_performance_is_membership_ground, holdable).
narrative_ontology:cs_axiom_grounding('60059e73-d84d-4a28-b049-d7da6d108ebc', social_performance_is_membership_ground, empirically_contingent).
narrative_ontology:cs_axiom('60059e73-d84d-4a28-b049-d7da6d108ebc', foundational, recognition_requires_external_validation).
narrative_ontology:cs_axiom_status(recognition_requires_external_validation, holdable).
narrative_ontology:cs_axiom_grounding('60059e73-d84d-4a28-b049-d7da6d108ebc', recognition_requires_external_validation, conventional).
narrative_ontology:cs_reference_frame('60059e73-d84d-4a28-b049-d7da6d108ebc', performance_based_category_gatekeeping).
narrative_ontology:cs_drift_state('60059e73-d84d-4a28-b049-d7da6d108ebc', institutional_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60059e73-d84d-4a28-b049-d7da6d108ebc', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women_conforming_to_norms).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_enforcement_network).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_nonconforming_presentation).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women_gender_nonconforming).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, individuals_failing_performance_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, trans_women_conditional_members).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_conditional_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trans women who present as feminine and are recognized by social networks gain conditional access to gendered spaces and categories. This access is contingent on sustained performance (appearance, mannerism, affect, social endorsement by cis peers). They bear the cost of constant performance monitoring and the risk of delegitimization if performance lapses or audience skepticism emerges. Exit is identity-locked: rejecting the performance means rejecting the category membership that structures their self-concept and social belonging.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_conditional_members, payer,
    powerless, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, trans_women_conditional_members, beneficiary).

% Cis women whose appearance, behavior, and affect align with social norms for femininity benefit from presumed category membership. They need not demonstrate belonging; presumption grants them access to gendered spaces, services, and social recognition without active defense of their credentials. Their own boundaries are partly policed by gatekeeping that also manages trans entry and exclusion of nonconforming cis women.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_conforming_to_norms, beneficiary,
    moderate, biographical, constrained, universal).

% Cis women whose presentation, affect, or social alignment deviates from gender norms face questioning of their category credentials. They may be mistaken for trans women or gender-nonconforming men, requiring them to do active work to defend gendered category membership. The gatekeeping machinery that measures authenticity-via-performance catches them as potential false positives, creating burden and social friction even when their biological credentials are uncontested.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women_gender_nonconforming, payer,
    moderate, biographical, constrained, universal).

% Distributed peers, colleagues, friends, and institutional actors who collectively judge performance, grant or withhold recognition, admit or exclude from gendered spaces and interactions. The enforcement is decentralized—no single authority, but coordinated through social feedback loops, repeated interaction patterns, institutional rules about bathroom access and sports categories, and the internalized monitoring individuals do on themselves. Enforcement network members benefit from having clear, performative boundary-markers that reduce ambiguity in social interaction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_enforcement_network, agenda_setter,
    organized, generational, mobile, universal).

% Trans women whose presentation does not align with cis women's normative appearance or whose transition is visible or recently begun face systematic exclusion from gendered category membership and the spaces that category opens. They cannot access category benefits and bear the full cost of the gatekeeping apparatus: exclusion from spaces, delegitimization, social friction, and the impossible bind of needing acceptance to prove belonging while being denied the performance stage on which to prove it.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_nonconforming_presentation, payer,
    powerless, biographical, trapped, universal).

% People across the gender spectrum whose performance (appearance, mannerism, affect, social recognition) falls below the threshold for unambiguous category membership bear continuous identity risk. They inhabit a state of conditional recognition that requires constant vigilance and performance maintenance. For some, this is a site of identity fluidity and self-creation; for others, it is a source of existential anxiety and social precarity.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, individuals_failing_performance_threshold, payer,
    moderate, biographical, identity_locked, universal).

% Actors who hold or advocate for the biological_sex_reading (membership grounded in chromosomal or reproductive anatomy criteria) are structurally excluded from this reading's framing. They would argue that performance cannot create or erase category membership and that gatekeeping based on perceived authenticity rather than objective markers is incoherent. Their voice would reframe the entire constraint as a false legitimacy narrative for overriding biological facts.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, biological_essentialist_advocates, excluded,
    organized, generational, mobile, universal).

% Actors who hold or advocate for the gender_identity_reading (membership grounded in self-declaration and internal gender identity) are also excluded from this reading's framing. They would argue that requiring external recognition and sustained performance for membership validity denies the sovereignty of self-knowledge and ties category membership to others' approval in ways that replicate the gatekeeping harms the constraint is claimed to address. Their voice would reframe performance-based gatekeeping as a coercive legitimation structure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_advocates, excluded,
    organized, generational, mobile, universal).

% Legislators, administrators, and institutional architects (schools, workplaces, sports bodies, healthcare systems) who make formal rules about which gendered spaces people may access, which categories appear on documents, and how credentials are verified. They observe the social_role_reading as one competing framework and implement policies that instantiate one reading or navigate ambiguously between readings. Their choices shape whether performance-based gatekeeping is legally codified or remains informal.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, institutional_rule_makers, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, social_enforcement_network).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a signal system for gendered category membership using socially-observable performance cues (appearance, behavior, affect, social endorsement, institutional integration) instead of requiring biological verification or internal psychological access. Enables real-time coordination in gendered social spaces without requiring DNA testing or psychological assessment at entry points.
% TRANSFER_FUNCTION: Transfers the burden of proving belonging from the social network (who would otherwise need biological or psychological verification) to the individual category-seeker, who must sustain performance and achieve social recognition. Also transfers social access and institutional benefits to those whose performance aligns with established norms for gendered categories.
% ABSENT_VOICES: Biological essentialists (who argue membership should be determined by immutable biological facts, not performance) and gender-identity advocates (who argue membership should follow self-declaration, not external recognition) are excluded from this reading's framework. Their absence is structural: the performance-based reading forecloses their core claims from being part of the same legitimation system.
% DISAPPEARANCE_RATIONALE: If performance-based gatekeeping for gendered category membership disappeared, either biological gatekeeping would become the primary mechanism (institutional adoption of sex-verification regimes), or identity-based self-declaration would become operative (self-reported gender on documents and in interaction), or hybrid systems would emerge. The specific extraction mechanism (monitoring performance, conditionalizing recognition, requiring presentation labor) would no longer structure the boundary. The coordination problem itself (how to signal gendered category membership) would persist but would be solved through different mechanisms.
% FOUNDING_PROBLEM: Social coordination in gendered spaces required ways to identify who belonged in which category without access to biological data or internal states at the moment of interaction. Appearance, behavior, social cues, and endorsement were the available mechanisms for signaling and recognizing gendered membership.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists and gender-studies scholars document the historical role of performance and recognition in constituting gendered categories (Goffman, Butler, Garfinkel). Healthcare providers in trans-competent practice document that performance-based acceptance significantly affects wellbeing and that performance-based gatekeeping creates medical and psychological harms. Trans women and gender-nonconforming people testify to the constant performance labor the constraint requires. However, biological essentialists attest that the founding problem was misdiagnosed—the true membership ground is biological and has always been accessible to institutions willing to verify it. Gender-identity advocates attest that gatekeeping never solved the founding problem; it created a new one (the requirement for external validation of internal knowledge). No unanimous corroboration—the founding problem is itself the site of the dispute.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the constraint genuinely solves coordination problems—gendered interaction requires some shared signal system—but the performance requirement extracts continuous labor from those seeking membership. The trajectory (0.38→0.52) reflects increasing technological and institutional codification of performance standards (social media, medical markers, institutional documents) that make the performance measurable and monitored, raising extraction as gatekeeping becomes more systematic. Suppression is higher (0.61) because gatekeeping—by definition—requires enforcement against those who do not meet performance thresholds. The enforcement is distributed and partly internalized (individuals monitor themselves), which is captured in the omegas rather than the scalar. Theater is moderate-low (0.28) because the stated coordination function (signal membership, reduce ambiguity in gendered interaction) is genuinely performed; the performative overhead is not theatrical but real activity. Accessibility_collapse (0.48) is moderate because alternatives exist (reject gendered categories, use biological/identity criteria) but are costly and not available to all agents (trans women cannot reject gendered category membership identity-locked; institutional spaces often codify performance-based gatekeeping, collapsing alternatives). Resistance is high (0.72) because substantial organized opposition exists from both biological essentialists and gender-identity advocates, and because individuals constantly experience friction with gatekeeping demands.
 *
 * PERSPECTIVAL GAP:
 *   Per-seat type divergence: A trans woman seat (d=0.9, identity-locked exit) should compute as snare or high-extraction tangled_rope; the enforcement network and benefiting cis women (d=0.1, mobile exit, organized power) compute as rope or low-extraction tangled_rope. The same constraint structure—distributed social gatekeeping based on performance—produces radically different experienced types depending on position. This divergence is exactly what the per-seat classification captures and is the point of seat-level analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women's directionality: high d (near 0.8–1.0). They are the primary targets—the constraint requires them to prove belonging through performance, gatekeeping exists primarily to manage their entry, and identity-locking makes exit impossible without rejecting gendered self-concept. Cis women gender-nonconforming: moderate-high d (0.5–0.7). They face questioning and performance demands, but retain presumption of biological membership; the gatekeeping catches them as collateral damage rather than primary targets. Cis women conforming: low d (0.15–0.25). They benefit from presumed membership and do not bear gatekeeping costs; they may benefit further from the exclusion of trans women and nonconforming cis women (gatekeeping protects their status). Social_enforcement_network: negative d / beneficiary (d approaches 0.0). They set the agenda, they are not subjected to performance demands, they benefit from the coordination function and from not having to verify membership biologically or psychologically. Directionality_overrides: none needed if the beneficiary/victim declarations and exit classifications are accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (coordinating gendered-category membership without real-time access to biological data) was likely genuine and important in historical contexts with less institutional infrastructure. The constraint may have emerged as a practical solution: appearance and behavioral signals were the available coordination mechanism. However, modern institutional infrastructure (documents, biological testing, legal regimes) provides alternatives that bypass performance-based gatekeeping entirely. The status of whether the founding problem remains live is itself contested: biological essentialists argue that modern biology makes performance-based gatekeeping unnecessary and incoherent; gender-identity advocates argue that the founding problem was never legitimately solved by gatekeeping, which only created new harms; institutional actors in some jurisdictions have moved toward self-declaration regimes (gender-identity reading), effectively declaring the founding problem solved differently. The measured theater ratio (0.28) is moderate, suggesting the stated coordination function is still performed but with growing performative overhead as enforcement codification increases. If theater_ratio rises significantly above 0.4, it would indicate that gatekeeping is now mostly maintenance theater and the coordination function has atrophied—piton candidate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_authenticity_ambiguity,
    'What determines whether a performance counts as ''authentic'' membership—is it about alignment with statistical norms of cis women''s presentation, or is it about the individual''s own intention and self-understanding expressed through behavior?',
    'Ethnographic study of social gatekeeping practices: who is challenged on their membership, under what conditions, and what criteria evaluators actually use. Post-hoc interviews with gatekeepers about their decision-making (do they reference ''typical femininity,'' ''individual authenticity,'' ''social integration,'' or something else?).',
    'If gatekeepers primarily measure conformity to statistical norms, the constraint is extraction of performance labor disguised as coordination. If they primarily measure alignment with stated identity, the boundary between this reading and the gender_identity_reading collapses. If they use some hybrid, the extraction is real but partially justified by coordination needs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_authenticity_ambiguity, empirical, 'Whether performance-based gatekeeping measures conformity to norms or alignment with stated identity.').

omega_variable(
    victim_structure_ambiguity,
    'Are trans women and cis women gender-nonconforming actually structural victims of the same constraint, or does the constraint harm them differently such that they should be modeled as distinct constraints with different ε values?',
    'Narrative and testimony from both groups about what they bear from performance-based gatekeeping. Comparison of the type/magnitude/duration of burden: trans women face exclusion from membership entirely unless performance is high; cis women face questioning and boundary challenges when performance is atypical. Different mechanisms may imply different constraints.',
    'If they are structurally distinct constraints (one about trans inclusion via performance, one about cis women''s conformity pressure), decompose into two stories linked by network.affects_constraints, each with its own ε, victims, and beneficiary structure. If they are one constraint, the ambiguity is about which group is the primary target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(victim_structure_ambiguity, conceptual, 'Whether performance-based gatekeeping for trans inclusion and for cis conformity are one constraint or two.').

omega_variable(
    identity_lock_mechanisms,
    'For trans women and gender-nonconforming individuals, what makes exit from performance-based gatekeeping identity-locked rather than merely constrained? Is it: (a) that rejecting performance requires rejecting the category itself and the self-understanding fused with it, (b) that the social network would not permit exit even if one wanted it, (c) that the costs of exit are experientially discontinuous from staying, or (d) some combination?',
    'Longitudinal interviews with people who have opted out of performance-intensive category participation: what did exit look like, what barriers were experienced. Narrative analysis of how trans women describe the relationship between gendered performance and self-concept.',
    'Each resolution shifts directionality and type classification for trans women seats. Identity-lock (a) means d=1.0 full target; trapped (b) shifts power classification; internalized suppression (c) requires omega on mechanism; combination is most likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanisms, empirical, 'What makes performance-based gendered category membership identity-locked for trans women.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.61) primarily structural (social exclusion, institutional rules, material gatekeeping) or internalized (individuals have adopted the gatekeeping logic and police themselves)?',
    'Post-exit suppression trajectory: if people who reject performance-based gatekeeping for gendered categories experience continued suppression (self-monitoring, shame, dysphoria around gender) after the structural constraint is removed, reclassify as partially internalized. Survey of individuals'' accounts of why they maintain performance: external pressure vs. internal alignment.',
    'If internalized, effective suppression is higher than the structural measure suggests and more persistent. The constraint carries itself with them after they leave gendered category participation, suggesting stronger identity-fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression in performance-based gendered categorization is structural, internalized, or both.').

omega_variable(
    reading_relation_logical_status,
    'Does the social_role_reading logically foreclose the biological_sex_reading and gender_identity_reading within a single framework, or do the three readings coexist as different parties'' commitments without logical conflict?',
    'Logical analysis: does accepting that gendered category membership is grounded in social performance and recognition logically require rejecting that it is also grounded in biology or identity? Can one consistently hold that performance is the operative social mechanism while biology or identity are the ''true'' grounds? Can one hold that identity is foundational but performance is the publicly available signal?',
    'If the readings logically foreclose each other, they are in genuine competition and only one can be right within any single framework. If they coexist, they answer different questions (what is true vs. what is socially operative). This determines whether reading_relations should be ''forecloses'' or ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_logical_status, conceptual, 'Logical status of the relationship between the three readings of the gendered_category_membership kernel.').

omega_variable(
    kernel_reading_as_constructed_constraint,
    'Is this reading—that gendered category membership is grounded in social performance and recognition—itself a false-summit candidate (a natural social mechanism presented as immutable law) or a genuine coordination constraint?',
    'Historical and anthropological analysis: how universal is performance-based gendered gatekeeping across cultures and time periods? Is it a necessary feature of any gendered social system, or one contingent instantiation? Are there functioning gendered social systems that allocate category membership via different mechanisms (biological verification, identity only, institutional assignment)?',
    'If performance-based gatekeeping is contingent rather than necessary, the constraint may be a false summit—a social construction that benefits (cis women, enforcement networks) while claiming to be a natural emergent property of gender itself. This would suggest false_summit_mountain classification is appropriate (beneficiaries are declared), triggering FSM override.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_as_constructed_constraint, conceptual, 'Whether performance-based gendered gatekeeping is a natural coordination mechanism or a constructed constraint with identifiable beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gend_tr_t0, projected).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__social_role_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(gend_tr_t5, projected).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(gend_tr_t10, observed).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__social_role_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(gend_tr_t15, observed).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gend_tr_t20, observed).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__social_role_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gend_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(gend_be_t0, projected).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__social_role_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(gend_be_t5, projected).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement_basis(gend_be_t10, observed).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__social_role_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement_basis(gend_be_t15, observed).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement_basis(gend_be_t20, observed).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__social_role_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement_basis(gend_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(gend_su_t0, projected).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__social_role_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(gend_su_t5, projected).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(gend_su_t10, observed).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__social_role_reading, suppression_requirement, 15, 0.59).
narrative_ontology:measurement_basis(gend_su_t15, observed).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(gend_su_t20, observed).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__social_role_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(gend_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested gendered_category_membership kernel. The biological_sex_reading and gender_identity_reading are sibling constraints in the same family, each grounding membership in different criteria (biology, identity, and performance/recognition respectively). All three stories link bidirectionally via network.affects_constraints because each reading's enforcement affects the legitimacy and operability of the others. The three readings cannot all be simultaneously enforced without contradiction—institutional policy must adopt one as primary, and the ε values differ substantially across readings (identity-grounding typically has lower ε and suppression than performance-based grounding, which in turn differs from biology-based grounding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__social_role_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
