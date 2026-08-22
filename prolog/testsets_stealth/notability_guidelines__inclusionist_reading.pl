% ============================================================================
% CONSTRAINT STORY: notability_guidelines__inclusionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__inclusionist_reading, []).

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
 *   constraint_id: notability_guidelines__inclusionist_reading
 *   human_readable: Notability Gate as Structural Exclusion Apparatus (Inclusionist Reading)
 *   domain: digital_commons_governance/knowledge_infrastructure/platform_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'notability_guidelines': the inclusionist reading, under which
 *   Wikipedia's Notability guideline (WP:N) operates as a structural
 *   gatekeeping apparatus that systematically excludes marginalized
 *   knowledge. The epsilon referent is the standing arrangement under
 *   contest: WP:N as it actually operates, assessed by this reading's own
 *   lights. A topic survives only if it carries significant coverage in
 *   sources judged reliable and independent, and the population of qualifying
 *   sources is dominated by institutional Western media and academic
 *   publishing. Communities outside those channels cannot supply the mandated
 *   currency, so their histories are deleted or never created, and the
 *   encyclopedia's composition reproduces the offline source-power hierarchy.
 *   Per the epsilon-invariance principle, the sibling readings
 *   (deletionist_reading: WP:N as necessary epistemic quality filter;
 *   deliberative_reading: WP:N as perpetually renegotiated boundary) are
 *   different constraints with different epsilon values, authored separately
 *   and linked through network.affects_constraints; they are not averaged
 *   into this file. The claimed type (snare) and the authored metrics are
 *   independent facts: the claim states what this reading holds structurally
 *   true; the metrics describe the arrangement's operation as this reading
 *   assesses it.
 *
 * KEY AGENTS:
 *   - institutional_knowledge_producers: Primary beneficiary (institutional/arbitrage) — their coverage is the sole admission currency; collects epistemic authority without running or bearing the gate
 *   - wikipedia_administrators: Agenda setter (institutional/identity_locked) — administers AfD and the guideline's interpretive layer; standing derives from fluent gatekeeping
 *   - marginalized_knowledge_communities: Primary target (powerless/trapped) — bears erasure; cannot self-supply 'reliable sources'
 *   - community_media_outlets: Secondary target (moderate/constrained) — primary documentation for excluded topics, discounted as unreliable
 *   - inclusionist_editors: Secondary target (moderate/identity_locked) — labor destroyed at AfD; bound to the project they oppose
 *   - bias_research_scholars: Analytical observer (moderate/analytical) — audits the gate's outputs from outside the benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, 0.78).
domain_priors:suppression_score(notability_guidelines__inclusionist_reading, 0.72).
domain_priors:theater_ratio(notability_guidelines__inclusionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(notability_guidelines__inclusionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__inclusionist_reading, snare).
narrative_ontology:human_readable(notability_guidelines__inclusionist_reading, "Notability Gate as Structural Exclusion Apparatus (Inclusionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__inclusionist_reading, "digital_commons_governance/knowledge_infrastructure/platform_constitutionalism").

domain_priors:requires_active_enforcement(notability_guidelines__inclusionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__inclusionist_reading, '40d26605-646d-44cb-b8c3-a6b4fad3669b').
narrative_ontology:cs_kernel_codification('40d26605-646d-44cb-b8c3-a6b4fad3669b', fixed_text).
narrative_ontology:cs_authority_grounding('40d26605-646d-44cb-b8c3-a6b4fad3669b', extraction).
narrative_ontology:cs_interpretation_layer_present('40d26605-646d-44cb-b8c3-a6b4fad3669b').
narrative_ontology:cs_reading_relation('40d26605-646d-44cb-b8c3-a6b4fad3669b', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('40d26605-646d-44cb-b8c3-a6b4fad3669b', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('40d26605-646d-44cb-b8c3-a6b4fad3669b', foundational, source_availability_is_not_significance).
narrative_ontology:cs_axiom_status(source_availability_is_not_significance, holdable).
narrative_ontology:cs_axiom_grounding('40d26605-646d-44cb-b8c3-a6b4fad3669b', source_availability_is_not_significance, empirically_contingent).
narrative_ontology:cs_axiom('40d26605-646d-44cb-b8c3-a6b4fad3669b', foundational, admission_criteria_compound_historical_erasure).
narrative_ontology:cs_axiom_status(admission_criteria_compound_historical_erasure, holdable).
narrative_ontology:cs_axiom_grounding('40d26605-646d-44cb-b8c3-a6b4fad3669b', admission_criteria_compound_historical_erasure, empirically_contingent).
narrative_ontology:cs_reference_frame('40d26605-646d-44cb-b8c3-a6b4fad3669b', institutional_source_admission_gate).
narrative_ontology:cs_drift_state('40d26605-646d-44cb-b8c3-a6b4fad3669b', post_systemic_bias_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('40d26605-646d-44cb-b8c3-a6b4fad3669b', '').
narrative_ontology:cs_kernel_id(notability_guidelines__inclusionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, marginalized_knowledge_communities).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, community_media_outlets).
narrative_ontology:constraint_victim(notability_guidelines__inclusionist_reading, inclusionist_editors).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, institutional_source_supremacy_doctrine).
narrative_ontology:constraint_vindicates(notability_guidelines__inclusionist_reading, legitimacy_through_institutional_validation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mainstream newsrooms, academic publishers, and reference-industry players whose coverage constitutes the sole admission currency. Every notability decision routes value to them: their judgment of what counts as significant coverage becomes binding on the encyclopedia, guaranteeing demand for their output and entrenching their position as arbiters of significance. They neither run the gate nor bear its costs; exit is meaningless because the arrangement subsidizes them wherever they operate.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, institutional_knowledge_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Volunteer administrators and experienced closing editors who run deletion discussions, maintain the guideline's sub-pages, and set enforcement norms through precedent. Their standing, service awards, and community authority derive from fluent administration of the gate. Leaving the project would forfeit a core social identity and accumulated reputation capital; most cannot imagine exiting even when they privately doubt specific deletions.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, wikipedia_administrators, agenda_setter,
    institutional, biographical, identity_locked, global).

% Indigenous nations, Global South localities, women in underdocumented fields, oral-history traditions, and minority-language publics whose histories live in community archives, oral transmission, and small-circulation press. When their topics come up for deletion, adjudication happens among editors citing source-availability rules they had no hand in writing; they typically learn of the decision after the article is gone. No alternative venue carries comparable reach or authority, and they cannot manufacture institutional coverage of themselves.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, marginalized_knowledge_communities, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, marginalized_knowledge_communities, excluded).

% Local newspapers, independent presses, diaspora broadcasters, and community archives whose reporting is the primary documentation for many marginalized topics. Reliability assessments discount them as self-interested or insufficiently rigorous, devaluing their epistemic standing; they cannot become 'reliable' no matter their accuracy, because the assessment criteria privilege scale and institutional affiliation.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, community_media_outlets, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__inclusionist_reading, community_media_outlets, excluded).

% Editors who create and defend articles on underdocumented topics. Their labor is repeatedly destroyed at deletion discussions; they spend effort on rescue tagging, source hunting, and appeals that usually fail. They stay because their Wikipedian identity and belief in the mission bind them to reform-from-within, absorbing the costs their own opposition generates.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, inclusionist_editors, payer,
    moderate, biographical, identity_locked, global).

% Researchers documenting demographic gaps in article coverage and editorship. They publish audits of the gate's outputs, contribute to movement-strategy consultations, and supply the external evidence base for reform proposals; they hold no vote inside the projects they study.
narrative_ontology:constraint_stakeholder(notability_guidelines__inclusionist_reading, bias_research_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__inclusionist_reading, institutional_knowledge_producers).
narrative_ontology:fixing_cost_class(notability_guidelines__inclusionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters spam, vanity biography, advertising, and unverifiable claims out of a volunteer-written encyclopedia at scale, giving millions of uncoordinated editors a shared test for which topics merit collective labor.
% TRANSFER_FUNCTION: Moves editorial labor and reader attention toward topics already documented by institutional media and academia, and moves epistemic authority to those institutional channels by making their coverage the sole admission currency; correspondingly moves the historical record away from communities documented only through community, oral, and small-circulation sources.
% ABSENT_VOICES: The subjects and descendants of deleted topics are almost never present at deletion discussions: no notification reaches them, language and account barriers exclude them, and the deliberation proceeds entirely among editors. Scholars from regions whose journals are poorly indexed are likewise absent from reliability assessments. Unanimity in deletion rooms arises where the people the decision is about were never admitted.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, hundreds of thousands of deleted drafts and red-linked topics would be recreated within months, the encyclopedia's topic composition would shift sharply toward previously excluded regions and populations, spam would surge until replacement filters emerged, and downstream consumers (search engines, AI training corpora, mirror sites) would inherit a materially different knowledge base.
% FOUNDING_PROBLEM: Early Wikipedia was drowning in vanity pages, promotional articles, and unverifiable claims; notability was devised as a proxy test for deciding which topics deserve shared volunteer attention without requiring an editor to judge significance case-by-case.
% FOUNDING_PROBLEM_CORROBORATION: Peer-reviewed audits of coverage gaps (gender-biography studies, Global South content analyses), the Wikimedia Foundation's own knowledge-gap strategy documents, and movement-strategy recommendations attest both that the anti-spam problem was real and that the current apparatus excludes systematically; this attestation comes from outside the benefiting parties (institutional producers and gate-administering editors), who instead attest that the founding problem remains fully live in its original form.
narrative_ontology:disappearance_verdict(notability_guidelines__inclusionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__inclusionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__inclusionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__inclusionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__inclusionist_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__inclusionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(notability_guidelines__inclusionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(notability_guidelines__inclusionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (epsilon = 0.78) because the arrangement converts source-power asymmetry into permanent differences in recorded existence: a topic without institutional coverage is not merely disadvantaged but removed. Suppression (0.72) is authored as a raw structural property — it is NOT scaled by power or scope; the engine scales only extractiveness (by directionality and spatial scope). It reflects the machinery the gate requires: AfD backlogs, speedy-deletion criteria, draftification, source blacklisting, and the operative norm that unsourced means unnotable. Theater ratio (0.45) is moderate: spam and vandalism control is genuinely functional, but a growing share of deletion activity adjudicates marginal-notability cases whose outcome tracks source availability rather than junk detection — performance of rigor over its substance. Accessibility collapse (0.60): alternatives exist (independent wiki farms, self-publishing, community archives) but none carry Wikipedia's epistemic authority, so understanding the gate does not open a usable exit. Resistance (0.55): sustained inclusionist factions, systemic-bias WikiProjects, organized edit-a-thons, and external scholarly criticism meet the gate continuously. boltzmann.coordination_type is resource_allocation: the gate's primary coordination function is allocating scarce shared editorial attention across candidate topics; the type floor prices inherent allocation overhead, and measured excess above it is attributed to the gate's extractive design rather than coordination cost. All three tracked series share one time grid ({0,5,10,15,20,25}); series endpoints equal the scalar base_properties values. Trajectories are monotonic ratchets, not cycles: extraction, theater, and enforcement intensity all accumulate as the corpus grows and the source gap compounds.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agenda-setter seat the same structure is careful stewardship: each deletion is an individual judgment, spam is real, and the corpus must be defended. From the payer seats the identical structure is a ratchet: every AfD closes against underdocumented topics, and the accumulation is visible only statistically, across thousands of individual 'reasonable' decisions. Institutional knowledge producers experience no constraint at all — the gate is invisible from inside the channels it privileges; it simply presents as 'how relevance works.' The engine computes these per-seat classifications from the structural data; this story's snare claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   institutional_knowledge_producers sit nearest the beneficiary pole (d near 0.0): the constraint subsidizes them by making their output the mandatory currency of admission, with arbitrage-grade insulation — they lose nothing under any reform short of abolition. marginalized_knowledge_communities sit nearest the target pole (d near 1.0): they pay in erased record and compounded invisibility, with trapped exit — no alternative venue carries comparable authority, and they cannot manufacture institutional coverage of themselves. community_media_outlets pay in devalued epistemic standing under constrained exit. inclusionist_editors pay in destroyed labor yet remain bound by identity lock, keeping their directionality high despite nominal insider status. wikipedia_administrators derive standing from administering the gate — a subsidy of status and purpose that places them well below symmetric even though they present as neutral executors. The beneficiary/victim declarations map onto these positions directly; no directionality overrides are needed because the derivation chain produces the correct relationships from the declared structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vanity-page and spam flooding — was real and remains partly live, which is precisely what makes this a snare rather than a mere malfunction: the coordination story is genuine enough to shield the extraction riding on it. A naive reading could call WP:N a rope (pure spam coordination) or a hybrid (real coordination plus asymmetric cost); the snare claim turns on three structural facts: the extraction is the primary steady-state output for the affected class (erasure, not friction), persistence depends on actively suppressing exits (no rival venue with authority; victims cannot self-supply the mandated currency), and identifiable victims exist. Mandatrophy risk runs the other way too: if the anti-spam function fully atrophied behind automated filters while deletion proceedings continued adjudicating culture, the apparatus would decay toward inertial theatrical maintenance. The founding_problem_status='contested' paired with disappearance_verdict='world_rearranges' flags that zombie-risk seam for the mismatch consumer. On the receipt surface: gain_flow names institutional_knowledge_producers because the constraint demonstrably makes their coverage the sole admission currency, transferring epistemic authority to their channels; fixing_cost is 'prohibitive' because the seats that could fix the gate (administrators, editing consensus) would have to overturn entrenched precedent, rewrite interlocked policies, and surrender the interpretive authority the current arrangement confers — a cost exceeding any benefit they individually collect from fixing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the notability_guidelines kernel; how would classification shift under the sibling readings?',
    'Cross-reading comparison: author deletionist_reading and deliberative_reading as separate constraint files and compare computed types, epsilon, and seat divergence across the family.',
    'Under the deletionist reading the same arrangement computes as a low-extraction quality filter; under the deliberative reading as a procedurally legitimated evolving process. Only the inclusionist reading yields the snare profile authored here; the divergence across the family is the datum.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which kernel, which reading, what siblings would change structurally.').

omega_variable(
    source_hierarchy_neutrality,
    'Is the alignment of ''reliable source'' with institutional Western media and academic publishing an accidental byproduct of quality screening, or constitutive of the standard itself?',
    'Citation audits correlating source-type exclusions with region, language, and topic demographics, controlling for measured accuracy of the excluded outlets.',
    'If exclusion tracks source-power rather than accuracy, the gate''s extraction is designed-in rather than incidental, hardening the snare classification; if accuracy explains the gap, extraction estimates fall toward hybrid-coordination territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_hierarchy_neutrality, empirical, 'Whether the source hierarchy embedded in the gate is contingent or constitutive.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (rules, deletion machinery, source scarcity) or internalized (communities and editors who have absorbed ''our knowledge is not notable'')?',
    'Post-liberalization trajectory: if sourcing relaxations for oral-history and community sources still fail to produce retained articles, internalization is carrying part of the suppression.',
    'If substantially internalized, effective suppression exceeds the structural measure and persists after rule reform; rule reform alone would not dismantle the gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized components of the gate''s suppressive force.').

omega_variable(
    counterfactual_composition_baseline,
    'What would the encyclopedia''s topic composition be under an admission rule neutral to source-channel power?',
    'Parallel-corpus experiments: seed wikis with relaxed notability thresholds and compare retention, vandalism rates, and quality trajectories against the gated baseline.',
    'Establishes the size of the extracted class: the set of topics whose absence is attributable to the gate rather than to genuine insignificance or spam risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_composition_baseline, empirical, 'Counterfactual baseline for measuring the gate''s exclusionary yield.').

omega_variable(
    coalition_formation_potential,
    'Can the powerless victim seats convert coalition power (edit-a-thons, thematic WikiProjects, GLAM partnerships) into enough leverage to force gate revision?',
    'Track retention rates of coalition-created articles and policy-change outcomes following organized campaigns across successive years.',
    'Sustained coalition success would shift the structure toward contested renegotiation with hybrid coordination/extraction dynamics; repeated failure despite mobilization confirms trapped exit and hardens the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_potential, empirical, 'Whether victim-class coalition formation can alter the gate''s equilibrium.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__inclusionist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_incl_tr_t0, notability_guidelines__inclusionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ng_incl_tr_t0, observed).
narrative_ontology:measurement(ng_incl_tr_t5, notability_guidelines__inclusionist_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement_basis(ng_incl_tr_t5, observed).
narrative_ontology:measurement(ng_incl_tr_t10, notability_guidelines__inclusionist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(ng_incl_tr_t10, observed).
narrative_ontology:measurement(ng_incl_tr_t15, notability_guidelines__inclusionist_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(ng_incl_tr_t15, observed).
narrative_ontology:measurement(ng_incl_tr_t20, notability_guidelines__inclusionist_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(ng_incl_tr_t20, observed).
narrative_ontology:measurement(ng_incl_tr_t25, notability_guidelines__inclusionist_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(ng_incl_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ng_incl_be_t0, notability_guidelines__inclusionist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ng_incl_be_t0, observed).
narrative_ontology:measurement(ng_incl_be_t5, notability_guidelines__inclusionist_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement_basis(ng_incl_be_t5, observed).
narrative_ontology:measurement(ng_incl_be_t10, notability_guidelines__inclusionist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(ng_incl_be_t10, observed).
narrative_ontology:measurement(ng_incl_be_t15, notability_guidelines__inclusionist_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(ng_incl_be_t15, observed).
narrative_ontology:measurement(ng_incl_be_t20, notability_guidelines__inclusionist_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement_basis(ng_incl_be_t20, observed).
narrative_ontology:measurement(ng_incl_be_t25, notability_guidelines__inclusionist_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement_basis(ng_incl_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ng_incl_su_t0, notability_guidelines__inclusionist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(ng_incl_su_t0, observed).
narrative_ontology:measurement(ng_incl_su_t5, notability_guidelines__inclusionist_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(ng_incl_su_t5, observed).
narrative_ontology:measurement(ng_incl_su_t10, notability_guidelines__inclusionist_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(ng_incl_su_t10, observed).
narrative_ontology:measurement(ng_incl_su_t15, notability_guidelines__inclusionist_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(ng_incl_su_t15, observed).
narrative_ontology:measurement(ng_incl_su_t20, notability_guidelines__inclusionist_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(ng_incl_su_t20, observed).
narrative_ontology:measurement(ng_incl_su_t25, notability_guidelines__inclusionist_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(ng_incl_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__inclusionist_reading, resource_allocation).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__inclusionist_reading, reliable_sources_policy).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'WP:N' decomposes into three structurally distinct constraints per the epsilon-invariance principle. deletionist_reading authors low extraction (quality filter, negligible rents); deliberative_reading authors process-legitimacy (extraction bounded by procedural fairness); this inclusionist_reading authors high extraction (structural exclusion). The deletionist reading is routinely cited as justification for the apparatus this reading indicts, so the deletionist story sits upstream of this one in the justification chain; reliable_sources_policy sits further upstream still, defining the source hierarchy the gate consumes. Each family member links the others through its own affects_constraints array.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
