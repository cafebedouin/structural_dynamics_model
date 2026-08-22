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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership via Social Performance and Recognition
 *   domain: social_ontology/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the social-role reading of gendered category
 *   membership: the claim that membership in gender categories (particularly
 *   'woman') is grounded in sustained social performance and recognition by
 *   others, not in immutable biological markers or self-declared identity
 *   alone. Within this reading, trans women are conditionally included when
 *   their sustained performance meets community recognition standards;
 *   gatekeeping is distributed across millions of micro-decisions by
 *   institutional and informal social actors; and the performance costs fall
 *   on those seeking recognition, while the boundary-maintenance labor falls
 *   partly on cis women and partly on distributed evaluators. The constraint
 *   exhibits both genuine coordination (practical problem of rapid
 *   categorization) and asymmetric extraction (conditional membership and
 *   performance burden). This is a kernel reading: the biological_sex_reading
 *   and gender_identity_reading are sibling constraints on the same kernel,
 *   not alternative frames within this story.
 *
 * KEY AGENTS:
 *   - trans_women: the conditionally included group, bearing identity-lock and performance costs to maintain category membership; identity is non-negotiable, exit from the gatekeeping structure is structurally closed
 *   - cis_women: moderate power, benefit from stable assumed membership but bear secondary boundary-maintenance labor; exit from gatekeeping participation is possible but socially costly
 *   - social_performance_evaluators: distributed organized actors (institutional staff, peers, platform moderators) who collectively enforce recognition standards; extract legitimacy and authority from adjudicating category truth
 *   - gender_nonconforming_individuals: powerless, identity-locked, excluded by construction because binary performance standard cannot be satisfied
 *   - institutional_researchers: analytical seat, documenting the performance standards and their variability across contexts and their lived costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.58).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.67).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership via Social Performance and Recognition").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social_ontology/political_philosophy").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, 'b0114176-8707-46f4-bdae-9c5369b2242d').
narrative_ontology:cs_kernel_codification('b0114176-8707-46f4-bdae-9c5369b2242d', distributed).
narrative_ontology:cs_authority_grounding('b0114176-8707-46f4-bdae-9c5369b2242d', practice).
narrative_ontology:cs_interpretation_layer_present('b0114176-8707-46f4-bdae-9c5369b2242d').
narrative_ontology:cs_reading_relation('b0114176-8707-46f4-bdae-9c5369b2242d', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0114176-8707-46f4-bdae-9c5369b2242d', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('b0114176-8707-46f4-bdae-9c5369b2242d', foundational, social_performance_constitutive_of_recognition).
narrative_ontology:cs_axiom_status(social_performance_constitutive_of_recognition, holdable).
narrative_ontology:cs_axiom_grounding('b0114176-8707-46f4-bdae-9c5369b2242d', social_performance_constitutive_of_recognition, empirically_contingent).
narrative_ontology:cs_axiom('b0114176-8707-46f4-bdae-9c5369b2242d', foundational, distributed_gatekeeping_as_coordination_mechanism).
narrative_ontology:cs_axiom_status(distributed_gatekeeping_as_coordination_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b0114176-8707-46f4-bdae-9c5369b2242d', distributed_gatekeeping_as_coordination_mechanism, instrumental).
narrative_ontology:cs_reference_frame('b0114176-8707-46f4-bdae-9c5369b2242d', performative_gender_recognition).
narrative_ontology:cs_drift_state('b0114176-8707-46f4-bdae-9c5369b2242d', contemporary_identity_recognition_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0114176-8707-46f4-bdae-9c5369b2242d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_majority_gatekeepers).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, social_performance_evaluators).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, cis_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, social_recognition_constitutive_of_identity).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, performative_theory_of_gender).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must continuously perform and sustain gendered social role to be recognized as women within any given context. Face daily evaluation by strangers, institutions, and social networks on credibility markers (presentation, mannerism, history, voice). Exclusion from women-coded spaces (shelters, bathrooms, sports, healthcare settings) depends on gatekeepers' assessment of whether performance meets community standards. Cost is constant vigilance, emotional labor in proving identity, and risk of sudden expulsion from spaces upon 'discovery' or changed perception. Exit is unavailable—identity itself is non-negotiable; only the social recognition is conditional.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women, payer,
    powerless, biographical, identity_locked, universal).

% Benefit from a category boundary that is maintained as exclusive and recognizable—membership appears stable and not subject to external performance judgment (gatekeeping is diffuse and often invisible to those assumed to belong). Simultaneously bear a secondary cost: category boundaries must be monitored and defended against boundary-crossing (the emotional labor of policing membership falls partly on members themselves, especially in women-only spaces). Some cis women oppose the gatekeeping structure; others actively enforce it. Exit from the gatekeeping role is possible but involves social friction and potential loss of assumed-membership protection.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cis_women, beneficiary,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, cis_women, payer).

% Collectively maintain and enforce the recognition standards by which gender category membership is adjudicated in daily interaction. Includes institutional actors (healthcare providers, legal registrars, shelter staff, sports administrators), informal community gatekeepers (peers in social groups), and digital platform moderators. They set the evidentiary threshold for what counts as 'passing' or 'credible' performance. Their power is distributed across millions of micro-decisions, but coherent enough to create a predictable social structure. They extract legitimacy and authority from enforcing boundaries and validating the category itself.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_performance_evaluators, agenda_setter,
    organized, generational, mobile, universal).

% Fail to meet the performance standards for any gender category and are thereby excluded or rendered ambiguous in gatekeeping contexts. Unlike trans women with viable performance pathways, gender-nonconforming individuals cannot satisfy the constraint through sustained performance—the boundary itself is constructed as binary, making conformity structurally impossible. Face exclusion from sex-segregated spaces by default and social friction everywhere. Identity is non-negotiable; exit from the gatekeeping structure is impossible by construction.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_individuals, payer,
    powerless, biographical, identity_locked, universal).

% Hold that category membership should be grounded in immutable biological markers (chromosomes, reproductive anatomy at birth), not performance. They are excluded from the social_role_reading's framing—their epistemic base is not recognized within this reading's logic. Their exclusion is structural: to adopt the social_role_reading is to reject the biological_sex_reading's core premise, so their voices are systematically unheard within this framework. They maintain institutional and legal positions from which to contest the social_role_reading despite this exclusion.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, biological_sex_reading_advocates, excluded,
    organized, generational, constrained, universal).

% Hold that category membership should be grounded in subjective identity and self-declaration, not performance. They too are excluded from the social_role_reading—they would argue that performance-based gatekeeping is unjust and that self-declaration should suffice. Their position is live in public discourse and legal reform movements but formally outside this reading's epistemic frame. They have growing institutional backing in some jurisdictions and are increasingly challenging gatekeeping authority.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_identity_reading_advocates, excluded,
    organized, generational, constrained, universal).

% Study the social performance and recognition standards that constitute gender categories in different institutional and cultural contexts. Document the variability of what counts as credible performance across healthcare, law, sports, and informal social settings. Record the lived costs of performance-based gatekeeping and the boundary-policing labor borne by multiple parties. Contribute empirical evidence to the ongoing dispute about whether social performance is the legitimate basis for gender category membership.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, institutional_researchers, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, social_performance_evaluators).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a practical social recognition problem: in the absence of fixed biological or legal markers universally accessible to every interaction, social actors coordinate on shared performance standards (presentation, behavior, social history) that enable rapid gender category identification in transactional, institutional, and intimate contexts. This distributed coordination avoids the need for constant biological verification or legal documentation of every gender assignment. The coordination function is genuine and economizes on verification costs.
% TRANSFER_FUNCTION: Moves legitimacy, authority, and category membership from trans women and gender-nonconforming individuals to cis majority gatekeepers and social_performance_evaluators. The transfer is paid in continuous performance labor by trans individuals (emotional energy, vulnerability to discovery, risk of sudden expulsion from spaces). Transfer of boundary-maintenance labor falls partly to cis women (who must monitor and defend category coherence) and partly to distributed evaluators (who must enforce standards against boundary-crossers and maintain the illusion of naturalness). Cis women gain stable, assumed category membership; trans women gain conditional, contingent membership; evaluators gain authority to adjudicate category truth and extract legitimacy from their gatekeeping role.
% ABSENT_VOICES: Biological_sex_reading advocates and gender_identity_reading advocates are structurally excluded from the social_role_reading's epistemic frame. Both groups have institutional power and organized representation but are not seated within this reading's logic. The biological_sex_reading community (sex-essentialists, radical feminists, conservative institutions) would argue that the reading disguises performance-based construction as if it were natural and immutable. The gender_identity_reading community (trans-affirmative advocates, identity-based activists, progressive institutions) would argue that the reading wrongly imposes a performance burden on trans individuals and that self-declaration should suffice without performance gatekeeping. Their absence means the reading is authored and defended without the voices of those who dispute its foundational premise most directly.
% DISAPPEARANCE_RATIONALE: If the social performance and recognition standard for gendered category membership disappeared—if gatekeeping ceased to operate on performance criteria—the social structure of gender would reorganize fundamentally. Institutional spaces (shelters, bathrooms, sports, healthcare) would either adopt alternative gatekeeping criteria (biological markers verified by documentation, or identity self-declaration, or no gatekeeping at all) or eliminate sex segregation entirely. The emotional labor burden on cis women in boundary maintenance would shift or vanish. Trans women's daily performance costs would drop dramatically or the category boundary would shift to a different criterion. The arrangement is not given by nature or law; its disappearance would matter materially to every stakeholder in different ways.
% FOUNDING_PROBLEM: How do social actors in face-to-face and institutional interaction rapidly and reliably categorize strangers and new community members as men or women when biological verification is impractical (most interactions do not permit bodily inspection or genetic testing) and legal documentation is incomplete or variable across jurisdictions? How do they coordinate on category boundaries without constant explicit negotiation? How do sex-segregated institutional spaces (shelters, bathrooms, sports, healthcare) function at scale without direct biological confirmation from every participant? What substitutes for biological or legal verification in routine social gatekeeping?
% FOUNDING_PROBLEM_CORROBORATION: Institutional actors (shelter workers, bathroom managers, sports administrators, healthcare providers) testify that rapid gender categorization is operationally necessary and is performed based on social cues and performance standards, not biological verification (which would be impractical at scale). Sociologists, anthropologists, and ethnographers document that gender categorization in interaction is performance-indexed and varies across cultural contexts. Trans individuals and activists outside the gatekeeping structure confirm that sustained social performance is empirically the mechanism by which they are or are not recognized as women in different settings. Legal systems testify that they rely partly on performance-based assessment when biological or documentary evidence is absent or contested. Behavioral scientists confirm that humans rapidly infer gender category from behavioral and presentational cues in the absence of explicit verification. The founding problem is attested by multiple structural positions (institutional need, empirical observation, lived experience), not only by those who benefit from gatekeeping.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58 at interval end) because the constraint generates real coordination value (rapid, distributed category recognition without constant verification) AND asymmetric costs (trans women's performance burden, cis women's boundary-labor burden). Extractiveness shows slight rise over the first three intervals (social norms tightening, performance expectations intensifying) then slight decline (increased social pressure and legal recognition challenges create countervailing forces). Suppression is high (0.67) because gatekeeping is maintained partly through social policing, fear of discovery, risk of sudden expulsion from women-coded spaces, and the distributed power of the evaluator network makes exit structurally closed for trans women. Theater_ratio is moderate (0.42) because the security/verification function is real but significant gatekeeping energy goes toward maintaining the distinction between 'genuine' and 'passing' women—the maintenance itself becomes performative. Accessibility_collapse is moderate-high (0.61): the performance standard seems natural and obvious from inside the cis majority perspective but is a constructed social criterion, not a natural law; some recognition of its constructedness exists even among cis gatekeepers. Resistance is high (0.72) because trans individuals and gender_identity_reading advocates actively contest and challenge the standard; lived resistance is substantial despite organized enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the social_performance_evaluators' distributed position, the constraint solves a practical institutional problem: how to manage sex-segregated spaces and make rapid gender assignments without constant biological documentation or legal paperwork that varies across jurisdictions. From trans women's position, the same structure is a mechanism of conditional inclusion where membership is always subject to evaluation, requiring continuous performance labor and carrying sudden expulsion risk when 'discovered' or when evaluators' standards shift. From cis women's position, the constraint is structurally ambiguous: it provides stable assumed membership (cis women are rarely evaluated for category authenticity) but imposes boundary-maintenance labor, especially in women-only spaces where cis women are often positioned as informal gatekeepers. From gender_identity_reading advocates' position, the constraint is pure extraction disguised as coordination—a mechanism to exclude or subordinate those whose identity performance fails to match evaluator expectations. The engine computes divergent directionality from the power/exit/role data; these stakeholder positions explain why divergence is structurally necessary, not a measurement error.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women have powerless position, identity_locked exit (the constraint's own framing forces the identity-lock: performance is demanded precisely because identity is non-negotiable), universal scope, and payer role (they bear performance costs and risk exclusion). Their d is near 1.0 (full target). Cis women have moderate power, constrained exit (can choose not to participate in gatekeeping but doing so invites social friction and loss of assumed membership status), universal scope, and hybrid beneficiary/payer role (benefit from assumed membership, pay boundary-maintenance labor). Their d is near symmetric, ~0.5, because coordination benefits and asymmetric costs roughly balance depending on individual choice. Social_performance_evaluators have organized power, mobile exit (can change roles or adopt different evaluation standards), universal scope, agenda_setter role (set and enforce the standards). Their d is near 0.0 (beneficiary/agenda-setter, they extract legitimacy and authority). Gender_nonconforming_individuals have powerless position, identity_locked exit (their identity cannot conform to the binary standard by construction), universal scope, payer role (excluded by default). Their d is near 1.0 (full target), in some contexts higher than trans women because exit is not merely conditional but structurally foreclosed. No directionality override needed; the structural data derives the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does have a genuine founding problem (rapid, reliable gender categorization at scale without constant documentation) and does solve it. However, the founding_problem_status is contested: some parties (biological_sex_reading advocates) argue that performance-based standards are not the solution and create the problem, and others (gender_identity_reading advocates) argue that self-declaration suffices without performance gatekeeping. The constraint persists not because it is the only solution but because evaluators have distributed power and gatekeepers have institutional interest in maintaining performance standards as the arbiter of category truth. This is not mandatrophy in the classical sense (founding problem dead, arrangement persists by inertia) but rather a live dispute about which solution is legitimate. The constraint is a tangled rope, not a piton, because the coordination function is real and the extraction function is asymmetric—both are present and active, not one degraded into the other. Mandatrophy resolution is not applicable here; the constraint is actively fought over, not theatrically maintained by invisible hands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_standard_variability_across_contexts,
    'What constitutes credible gender performance varies substantially across institutional and cultural contexts—what counts as ''passing'' in one setting may not transfer to another, and the performance standards themselves change over time. Is this variability a feature of a genuinely distributed, context-responsive coordination mechanism, or evidence that the mechanism is unstable and culturally specific rather than natural or universal?',
    'Ethnographic and comparative historical documentation of how gender performance standards differ across: (1) different institutions (shelters, healthcare, sports, legal registries); (2) different geographic regions and cultural contexts; (3) different historical periods. Track whether the variability is systematic or noise, and whether gatekeepers acknowledge the variability or treat their own standards as universal.',
    'High variability acknowledged would support the interpretation that social_role_reading is a cultural construct, not a natural law for gender categorization. This would strengthen the case for alternative readings (biology-based or identity-based) as equally valid. Low variability or unacknowledged variability would suggest that performance standards are treated as natural despite their constructedness, supporting a false-summit analysis (treating constructed criteria as if they were natural law).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_standard_variability_across_contexts, empirical, 'Variability of performance standards across institutional and cultural contexts.').

omega_variable(
    identity_lock_internalization_mechanism,
    'Trans women''s identity_locked exit option reflects that gender identity itself is non-negotiable and cannot be abandoned as an exit strategy. But within the social_role_reading frame, is the lock mechanism (1) structural—external barriers preventing exit even if identity were negotiable, or (2) internalized—the person''s identity is so fused with their gender self-concept that negotiating exit is psychologically unavailable, even when legally or institutionally possible?',
    'Post-transition or post-recognition interviews with trans women about whether they would abandon their gender identity if gatekeeping pressure disappeared vs. whether they would maintain it. Study what happens when gatekeeping formally ends (legal recognition, jurisdictional changes) to see if internalized exit remains locked even when structural barriers lift. Compare to identity-locked cases in other domains (religious conversion, organizational identification, cult deconversion) to identify whether the suppression mechanism is structural, internalized, or both.',
    'If the lock is primarily internalized, the suppression is higher than the structural measure suggests (the target carries the suppression with them even after external barriers are removed, making effective suppression closer to 1.0). If the lock is primarily structural, the suppression captures the constraint accurately and removal of external barriers would dramatically reduce effective suppression. If both, the suppression measurement is accurate but the exit option itself is partially misfiled—internalized identity fusion might better fit as part of the power atom (powerless + internalized lock = even more constrained than structural powerlessness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_mechanism, empirical, 'Whether identity-lock is structural or internalized or both.').

omega_variable(
    ambiguous_victim_structure,
    'Both trans women and cis women bear costs within this constraint, but for different reasons. Trans women bear performance costs and expulsion risk. Cis women bear boundary-maintenance labor and risk of category confusion if boundaries are not policed. Are these genuinely symmetric costs, or is the distribution of costs asymmetric (one group bearing higher costs even though both are listed as victims)?',
    'Comparative measurement of: (1) time and emotional energy spent on performance/boundary-maintenance by each group; (2) institutional and social risks if each group stops participating (cis women stop policing boundaries, trans women stop performing); (3) bargaining power each group has to renegotiate the constraint terms. Compare stated preferences about whether each group would maintain the constraint if the other group''s role changed.',
    'If costs are actually asymmetric with trans women bearing substantially higher costs, the constraint is closer to a snare than a tangled_rope—the ''cooperation'' is not mutual and the coordination benefit is not shared. If costs are genuinely symmetric, the tangled_rope classification holds and both groups have leverage to renegotiate. If asymmetry varies across institutional contexts, the type might differ by seat (piton from some positions, tangled_rope from others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguous_victim_structure, empirical, 'Whether victim costs are symmetric or asymmetric between trans and cis women.').

omega_variable(
    reading_foreclosure_via_biological_sex_axiom,
    'Does the social_role_reading logically foreclose the biological_sex_reading, or do they genuinely coexist? If a party holds that ''biological sex is the ultimate determinant of gender category membership'' AND simultaneously holds that ''social performance creates credible category membership'', do these cohere in a single framework or is at least one abandoned when the tension is explicit?',
    'Examine cases where biological_sex_reading advocates have shifted to social_role_reading framing or vice versa: does the shift happen gradually (suggesting coexistence with strategic positioning), abruptly (suggesting logical incompatibility once made explicit), or never (suggesting genuine coexistence in separate communities)? Analyze formal statements from each reading''s institutional defenders to see if they explicitly acknowledge or deny the coexistence.',
    'If foreclosure is real, the relation is ''forecloses'' not ''coexists_with'', and one reading will not survive long-term pressure. If coexistence is real, both readings remain live and the constraint landscape is genuinely contested. This affects the type of the biological_sex_reading and gender_identity_reading siblings: if they foreclose each other, one or both may resolve to ''mountain'' (falsely—they would be false summits imposed by epistemic closure); if they coexist, all three remain live options and the constraint family remains unsettled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_biological_sex_axiom, conceptual, 'Whether the social_role_reading logically forecloses the biological_sex_reading or genuinely coexists with it.').

omega_variable(
    false_summit_risk_social_performance,
    'Is the social_role_reading''s appeal to ''natural'' performance standards a false natural law? The reading claims performance-based recognition is the practical solution to a coordination problem, but this claim may naturalize what is actually a constructed social standard enforced by distributed power. If biological_sex_reading advocates are wrong and social_role_reading advocates are wrong, is the truth closer to gender_identity_reading or to something else entirely?',
    'Historical evidence from cultures and periods where gender categories were organized on different bases (role-based, identity-based, multiple genders, fluid categorization). If social performance was the historical mode only in specific contexts, not universal, then it is culturally contingent, not natural. Examine whether the performance standard changes when power relationships change (e.g., when trans people gain institutional recognition, do evaluation criteria shift to protect existing boundaries—evidence of motivated gatekeeping rather than natural coordination).',
    'If social_role_reading is a false summit (a constructed arrangement defended by distributed power but treated as natural coordination), the constraint should be reclassified: possibly as snare (pure extraction with a coordination cover story) or as piton (a formerly functional arrangement now maintained by theater). The beneficiaries would be not those who benefit from coordination but those who benefit from the particular power distribution gatekeeping preserves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_risk_social_performance, conceptual, 'Whether social-performance-based gender categorization is a natural coordination solution or a false summit defended by distributed power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(gend_tr_t0, observed).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(gend_tr_t8, observed).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(gend_tr_t16, observed).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(gend_tr_t24, observed).
narrative_ontology:measurement(gend_tr_t32, gendered_category_membership__social_role_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement_basis(gend_tr_t32, observed).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__social_role_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(gend_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(gend_be_t0, observed).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(gend_be_t8, observed).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(gend_be_t16, observed).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(gend_be_t24, observed).
narrative_ontology:measurement(gend_be_t32, gendered_category_membership__social_role_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement_basis(gend_be_t32, observed).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__social_role_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(gend_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(gend_su_t0, observed).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(gend_su_t8, observed).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__social_role_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(gend_su_t16, observed).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__social_role_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(gend_su_t24, observed).
narrative_ontology:measurement(gend_su_t32, gendered_category_membership__social_role_reading, suppression_requirement, 32, 0.68).
narrative_ontology:measurement_basis(gend_su_t32, observed).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__social_role_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(gend_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__social_role_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gendered_category_membership__gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading family under the kernel gendered_category_membership. All three readings (biological_sex_reading, gender_identity_reading, social_role_reading) share the same referent (the system by which gender category membership is assigned and policed) but instantiate different structural claims about the legitimate basis of that membership. The ε values differ substantially across readings: biological_sex_reading treats the system as natural law (low ε); gender_identity_reading treats it as extraction from those forced to perform (high ε); social_role_reading treats it as tangled coordination with asymmetric costs (moderate ε). The three constraints are linked because each reading's institutional success influences the others' operating conditions: if gender_identity_reading gains legal recognition (self-declaration suffices), the social_role_reading's gatekeeping becomes harder to maintain. If biological_sex_reading dominates, social_role_reading becomes inert (performance stops mattering). The family is unresolved; all three readings remain live in contemporary discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
