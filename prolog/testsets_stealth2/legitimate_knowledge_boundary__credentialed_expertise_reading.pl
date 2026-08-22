% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Peer Review Boundary on Legitimate Knowledge
 *   domain: epistemology/science-and-technology-studies/political-theory
 *
 * SUMMARY:
 *   An interlocking apparatus — credential-granting institutions,
 *   peer-reviewed journals, editorial boards, funding panels, promotion
 *   committees — draws the line between knowledge and mere opinion. This
 *   story instantiates ONE reading of the legitimate_knowledge_boundary
 *   kernel: the credentialed_expertise_reading, on which the line is drawn by
 *   methodological rigor validated through credentialed peer review. Per the
 *   epsilon-referent rule, epsilon describes the standing gatekeeping
 *   arrangement as this reading assesses it — never the pluralist or
 *   coproduction arrangements the siblings would install. The reading affirms
 *   the filtering function, and the authored metrics nonetheless record the
 *   extraction running through the same pipes: publisher margins on publicly
 *   financed output, uncompensated review labor, rigor enforced harder
 *   against challengers than incumbents, and warrants excluded by definition.
 *   Claim and metrics are independent facts: claimed_type tangled_rope states
 *   the structural truth that a genuine coordination function and asymmetric
 *   extraction operate through one machinery; the metrics were authored from
 *   descriptive evidence, not tuned to any predicted verdict.
 *
 * KEY AGENTS:
 *   - - commercial_journal_publishers: agenda setter (institutional/arbitrage) — administers the gate, collects the monetary rents
 *   - - credentialed_experts: primary beneficiary and enforcement workforce (organized/identity_locked)
 *   - - early_career_researchers: primary payer among insiders (moderate/constrained)
 *   - - independent_scholars: locked-out payer (powerless/trapped)
 *   - - experiential_knowledge_holders: excluded voice (powerless/trapped)
 *   - - public_research_funders: double-paying institutional seat (institutional/constrained)
 *   - - general_public: excluded consumer of the truth-proxy (moderate/trapped)
 *   - - sts_epistemology_analysts: analytical observer — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.62).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.6).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Peer Review Boundary on Legitimate Knowledge").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science-and-technology-studies/political-theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'b0fe4638-92f6-42cb-a8bd-d0d76502f4ad').
narrative_ontology:cs_kernel_codification('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', formalized).
narrative_ontology:cs_authority_grounding('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', expertise).
narrative_ontology:cs_interpretation_layer_present('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad').
narrative_ontology:cs_reading_relation('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', legitimate_knowledge_boundary__experiential_pluralism_reading, forecloses).
narrative_ontology:cs_reading_relation('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', foundational, methodological_rigor_necessary_for_legitimate_knowledge).
narrative_ontology:cs_axiom_status(methodological_rigor_necessary_for_legitimate_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', methodological_rigor_necessary_for_legitimate_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', foundational, credentialed_peer_review_is_the_validating_mechanism).
narrative_ontology:cs_axiom_status(credentialed_peer_review_is_the_validating_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', credentialed_peer_review_is_the_validating_mechanism, conventional).
narrative_ontology:cs_reference_frame('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', methodological_rigor_expert_consensus).
narrative_ontology:cs_drift_state('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', post_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0fe4638-92f6-42cb-a8bd-d0d76502f4ad', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, commercial_journal_publishers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_professional_societies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, early_career_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_scholars).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, public_research_funders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, public_research_funders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the journal titles through which legitimacy is conferred, set acceptance and rejection through editor appointments, and require copyright transfer on submission. Collect subscription fees, article processing charges, and reprint sales on output financed by others, while receiving peer review and much editing labor unpaid. Prestige rankings tied to their titles lock institutions into bundled contracts; the portfolio can be shifted across jurisdictions and customers at will.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, commercial_journal_publishers, agenda_setter,
    institutional, generational, arbitrage, global).

% Tenured faculty, licensed professionals, and laboratory leaders whose authority, income, and standing flow from the credential-and-review system. They staff editorial boards, review panels, and grant committees — supplying the enforcement labor, largely without payment — and their testimony carries weight in courts, agencies, and media precisely because the boundary certifies them. Leaving the system would mean forfeiting the professional identity the credential constitutes; retirement rather than resignation is the usual exit.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_experts, agenda_setter).

% Membership organizations that collect dues, conference registration, and journal income, and supply the volunteer governance that operates review. Their relevance depends on the boundary remaining the route to recognition; several derive a large share of operating budget from the publication side of the arrangement they help administer.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, scientific_professional_societies, beneficiary,
    organized, generational, constrained, continental).

% Doctoral students and postdoctoral and junior faculty who produce both the papers and much of the review labor. They bear publish-or-perish evaluation, rejection lotteries measured in years, article processing charges, and short-term contracts renewed contingent on outlet prestige. Years of specialized training are sunk into the channel; exiting means leaving research altogether, so they absorb the terms offered.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, early_career_researchers, payer,
    moderate, biographical, constrained, global).

% Serious inquirers without institutional affiliation. Manuscripts face desk rejection for lack of credential or host institution, library access is unavailable, and conference participation is unaffordable or invitation-gated. There is no funded pathway by which their work acquires legitimacy inside the boundary; the door is the thing they lack.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, independent_scholars, payer,
    powerless, biographical, trapped, regional).

% Patients, craftspeople, farmers, indigenous and local communities whose knowledge has been validated by generations of use and outcome. Inside this boundary their testimony enters only as anecdote or raw data to be certified by others; it cannot itself constitute validation. What excludes them is not a rule they broke but the boundary's definition of warrant — there is no application they could file.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_holders, excluded,
    powerless, generational, trapped, global).

% Governments, agencies, and foundations that finance the research, then pay subscription fees and processing charges to read the results — paying twice for the same knowledge. They also depend on the boundary's seal when acting on expert advice, which makes them cautious reformers: mandates for open access have been issued, but the trust infrastructure the boundary provides is not something they can replace quickly.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, public_research_funders, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, public_research_funders, beneficiary).

% Consumes expert consensus as a ready-made proxy for truth, pays for it through taxation and through paywalled access, and has no procedural role in validating or contesting what crosses the boundary. Dissent from consensus reads, inside the framework, as ignorance or denialism; the framework offers no legitimate station from which a layperson could dispute a finding.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, general_public, excluded,
    moderate, generational, trapped, global).

% Historians, philosophers, and social scientists of knowledge who study how the boundary is built, maintained, and contested. They document citation asymmetries, review outcomes, and the history of professionalization; they hold no admission rights of their own and neither collect from nor bear the arrangement's costs.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, sts_epistemology_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, commercial_journal_publishers).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the trust-and-filtering problem of modern knowledge production: millions of claims are made annually, and someone must separate the reliable from the unreliable before society acts on them. Shared methodological standards make findings commensurable across laboratories and generations; peer review catches errors before wide dissemination; credentials signal who has been trained into the standards. Whatever else it does, the arrangement does coordinate inquiry at civilization scale.
% TRANSFER_FUNCTION: Moves epistemic authority and career security toward credentialed insiders; moves unpaid review and editing labor from researchers to publishers; moves subscription and processing revenue from funders, libraries, and authors to publishers; and moves decision rights over what counts as knowledge to editors, reviewers, and credentialing bodies.
% ABSENT_VOICES: Experiential knowledge holders, independent scholars, and the lay public would object that the boundary forecloses valid ways of knowing and prices verification beyond reach — but they are absent precisely because the boundary's enforcement consists in not admitting their testimony. Historically, communities whose knowledge was collected as data and then dismissed sit furthest outside the room.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight, the knowledge economy would reorganize around whatever replaced it: reputational markets would form among replication brokers, open annotation layers, funder consortia, and community validators; publishers would lose the rents that ride on exclusivity; hospitals, courts, and regulators would need new warrants before acting on claims. Whether the replacement filtered better or worse, nothing about current arrangements — careers, contracts, curricula, advisory chains — survives intact.
% FOUNDING_PROBLEM: Professionalization in the nineteenth and twentieth centuries: separating inquiry from cranks, charlatans, and unfalsifiable speculation; building a trust infrastructure so that medicine, engineering, and policy could act on expert advice without each citizen re-deriving the evidence; making claims commensurable across laboratories through shared method.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: historians of science document the professionalization movement's stated aims independently of today's incumbents; public-health agencies attest ongoing harms from unfiltered misinformation; and the arrangement's sharpest critics — open-access advocates, replication researchers, STS scholars — concede the filtering problem is real while disputing that this apparatus solves it. Publisher and society attestations of the problem are self-interested and discounted accordingly.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the filtering service is real, but a large and growing share of the value flowing through the boundary accrues to parties who did not produce the underlying work — documented publisher margins on publicly funded research, subscription and processing charges levied twice on funders, and review labor donated under implicit compulsion. Suppression 0.60 and rising: the enforcement ratchet (impact-factor regimes, publish-or-perish evaluation, integrity bureaucracies, desk-rejection norms) hardened over the interval; distribution-side loosening such as preprints partially offsets but does not reach the validation gate itself, which is where this constraint lives. Theater ratio 0.42 and rising: a growing fraction of review activity performs rigor rather than producing it — statistical ritualism, token reviewer counts, metric gaming, salami publication — consistent with Goodhart drift around the impact factor. Accessibility collapse 0.65: once the boundary is accepted, non-credentialed warrants collapse in legitimacy terms inside institutional settings, though they persist socially, which keeps the value below mountain-grade. Resistance 0.55: open-access campaigns, preprint culture, replication-crisis critique, and predatory-journal exit routes meet the boundary with real, organized pushback. The three tracked series share one time grid (points map approximately to five-year bands from the mid-1970s to the mid-2020s); suppression_requirement is tracked because the story's narrative is precisely an enforcement-capacity buildup. Coalition note: the powerless payer seats (independents, experiential holders) have shown coalition capacity — citizen-science networks, community-review experiments — which is why their effective power is rated above zero despite individual trapping.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structure. From the credentialed expert's chair the arrangement presents as a functioning quality filter that certifies their own competence — coordination shading toward rope, with identity fusion masking the labor they donate. From the early-career chair it presents as a toll road with a function somewhere behind the tolls. From the independent scholar's and experiential holder's chairs it presents as a closed door with no handle — extraction with no offsetting service rendered to them at all. From the publisher's chair it presents as a neutral market in prestige. The engine computes these per-seat classifications from the structural data; this story's claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Publishers sit nearest the full-beneficiary pole (arbitrage-grade exit, they set and collect). Professional societies sit close behind. Credentialed experts derive low directionality from their beneficiary declaration, correctly so in net terms, though they also supply the unpaid enforcement labor — a nuance the power-atom-keyed override surface cannot express without mislabeling the societies, so it is recorded here rather than as an override. Funders sit near symmetric: they pay twice yet consume the trust seal. Early-career researchers sit well toward the target pole; independent scholars further; experiential knowledge holders nearest the full-target pole, since the boundary's entire operation, for them, is delegitimation. The public sits moderately toward the target pole: subsidized consumers with no verification rights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — quackery, misinformation, and the need for a social trust filter on knowledge claims — remains live, so no mandatrophy is declared and none is due. The classification earns its keep by blocking two symmetrical mislabels: reading the arrangement as pure coordination (rope) would erase the documented rents, the double-payment loop, and the locked-out seats; reading it as pure extraction (snare) would erase the filtering function that even the arrangement's critics implicitly rely on when they demand better review rather than none. Tangled rope keeps both halves on the books, and the temporal series is positioned to detect the drift that matters: continued extraction accumulation with rising theater would tip the balance toward snare, while successful reform (diamond open access, registered reports, coproduction pilots) would pull it back toward rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the legitimate_knowledge_boundary kernel (reading: credentialed_expertise_reading). What structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Comparative classification across the three reading-stories in the family: locate the disagreement at whether credential validation is necessary, sufficient, or neither for epistemic legitimacy, and observe how each reading redistributes the beneficiary and victim sets.',
    'Under the experiential sibling, experiential knowledge holders move from excluded/target to validator, and this reading''s enforcement machinery becomes the extraction object; under the hybrid sibling, a coproduction requirement is added that this reading''s venues currently cannot certify, shifting extraction toward whoever controls the integration process. Classification of the whole family moves with the resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the readings disagree.').

omega_variable(
    consensus_truth_proxy_reliability,
    'Does credentialed expert consensus actually track truth well enough to serve as the operative truth-proxy, at what rate does published consensus fail replication or reversal, and does the failure rate differ by field?',
    'Large-scale replication projects, adversarial collaborations, and forecast tournaments scoring expert consensus against preregistered replications and longitudinal outcomes, disaggregated by discipline and study design.',
    'A high failure rate would mean the boundary misclassifies knowledge at scale — certifying falsehood and excluding truth — converting much of the measured coordination function into overhead and pushing the arrangement toward pure extraction; a low rate would strengthen the coordination reading and raise the justified floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_truth_proxy_reliability, empirical, 'Reliability of expert consensus as a truth-tracking instrument.').

omega_variable(
    symmetry_of_rigor_enforcement,
    'Is methodological rigor enforced symmetrically across submissions, or asymmetrically — harder against paradigm-challenging, heterodox, and non-affiliated work than against confirmatory work from credentialed insiders?',
    'Audit of editorial outcomes: rejection and revision-severity rates matched on methodological quality but varying on paradigm-alignment, author affiliation, and institutional prestige; citation-lag analysis of challenged findings.',
    'Demonstrated asymmetry would identify a protection mechanism for incumbents riding on the coordination language, raising effective extraction on challenger seats and supporting reclassification pressure toward snare for those seats; symmetry would support the tangled-rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_of_rigor_enforcement, empirical, 'Whether the gate enforces the standard or the hierarchy.').

omega_variable(
    structural_vs_internalized_delegitimization,
    'Is the exclusion of non-credentialed knowers structural (venue access, funding gates, affiliation screens) or internalized (aspiring knowers who accept the inferiority of their own warrant and self-censor without being barred)?',
    'Post-barrier trajectory studies: track independent scholars and experiential-knowledge programs granted formal review access — if self-disqualification and deference persist after the structural gate opens, the suppression is partly internalized; survey-based measurement of warrant self-assessment across credential strata.',
    'If internalized, effective suppression exceeds the structural measure and persists after reform — the boundary would continue operating through its former targets'' own assessments; if structural, opening the gate dissolves the effect and reform is cheaper than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_delegitimization, empirical, 'Structural versus internalized mechanism of epistemic exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(legi_tr_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(legi_tr_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(legi_tr_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(legi_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(legi_be_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(legi_be_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(legi_be_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(legi_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(legi_su_t10, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(legi_su_t20, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(legi_su_t30, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(legi_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, hybrid_coproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what counts as legitimate knowledge' decomposes, per the epsilon-invariance principle, into three structurally distinct boundary arrangements corresponding to the three readings of the legitimate_knowledge_boundary kernel. This file instantiates the credentialed_expertise_reading; the experiential_pluralism_reading and hybrid_coproduction_reading files instantiate the siblings. The epsilon values differ across the family because the beneficiary/victim sets differ structurally, not because one constraint is measured different ways: this reading's victim set includes experiential knowledge holders and independents whom the pluralist reading treats as validators, and the hybrid reading adds a coproduction requirement this reading lacks. This reading is upstream in the family — the most institutionally established — and its venues, standards, and prestige economy shape the operating conditions of the hybrid sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
