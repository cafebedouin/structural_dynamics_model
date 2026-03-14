% ============================================================================
% CONSTRAINT STORY: institutional_epistemic_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_epistemic_authority, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_epistemic_authority
 *   human_readable: Institutional Epistemic Authority and Knowledge Gatekeeping
 *   domain: epistemology/institutional_authority
 *
 * SUMMARY:
 *   Institutional epistemic authority represents the structural arrangement
 *   by which credentialed institutions (universities, professional colleges,
 *   research institutes) monopolize the legitimacy to produce, evaluate, and
 *   certify knowledge claims. This constraint operates across all human
 *   knowledge domains — medicine, engineering, law, natural sciences, social
 *   sciences, and humanities. It exhibits genuine coordination functions
 *   (peer review, quality assurance, cumulative knowledge building) alongside
 *   asymmetric extraction (credential monopolies, rent-seeking through
 *   prestige gatekeeping, suppression of non-institutional knowledge
 *   producers). The constraint is neither pure coordination (Rope) nor pure
 *   extraction (Snare) but a hybrid that evolves over time. Theater ratio has
 *   increased from 0.42 to 0.68 over the past 50 years as institutional
 *   prestige and credential inflation have outpaced verification function —
 *   impact factors are gamed, peer review is arbitrary, and accreditation has
 *   become ritual. Extractiveness has similarly grown from 0.35 to 0.58,
 *   reflecting credential scarcity rents and institutional profit
 *   maximization. The constraint demonstrates all six classification types
 *   depending on observer position: pure coordination (credentialed
 *   institutions), pure extraction (powerless excluded producers), mixed
 *   coordination-extraction (early-career researchers and professional
 *   licensing bodies), temporary coordination (open knowledge movement),
 *   degraded ritual (accreditation system), and false natural law (analytical
 *   observer claiming verification requires institutional concentration).
 *
 * KEY AGENTS:
 *   - Credentialed Institutions: Primary beneficiary (institutional/arbitrage) — universities, research institutes, professional colleges capture monopoly rents through credential scarcity and prestige gatekeeping
 *   - Professional Licensing Bodies: Secondary beneficiary (organized/constrained) — medical boards, engineering associations, legal bar associations coordinate genuine safety standards while extracting monopoly rents through labor supply restriction
 *   - Early-Career Institutional Researchers: Secondary victim (moderate/constrained) — benefit from credential-sharing but bear asymmetric extraction through labor value differential and publication pressure
 *   - Excluded Knowledge Producers: Primary victim (powerless/trapped) — cannot access publication, peer review, funding, or legitimacy without institutional affiliation; investment in knowledge production yields no authority without institutional stamp
 *   - Alternative Epistemic Traditions: Tertiary victim (powerless/constrained) — indigenous knowledge, practical crafts, community science, lived experience are suppressed as non-credentialed despite epistemic validity
 *   - Marginalized Communities: Tertiary victim (powerless/trapped) — lack access to institutional credentialing and decision-making; their knowledge and preferences are subordinated to institutional expertise
 *   - Open Knowledge Movement: Organized actor (organized/constrained) — open-access advocates, citizen science networks, blockchain credentialing systems building alternative verification pathways; see institutional gatekeeping as temporary
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to knowledge production itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_epistemic_authority, 0.58).
domain_priors:suppression_score(institutional_epistemic_authority, 0.65).
domain_priors:theater_ratio(institutional_epistemic_authority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_epistemic_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_epistemic_authority, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_epistemic_authority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_epistemic_authority, tangled_rope).
narrative_ontology:human_readable(institutional_epistemic_authority, "Institutional Epistemic Authority and Knowledge Gatekeeping").
narrative_ontology:topic_domain(institutional_epistemic_authority, "epistemology/institutional_authority").

domain_priors:requires_active_enforcement(institutional_epistemic_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_epistemic_authority, credentialed_institutions).
narrative_ontology:constraint_beneficiary(institutional_epistemic_authority, professional_licensing_bodies).
narrative_ontology:constraint_victim(institutional_epistemic_authority, knowledge_producers_without_credentials).
narrative_ontology:constraint_victim(institutional_epistemic_authority, alternative_epistemic_traditions).
narrative_ontology:constraint_victim(institutional_epistemic_authority, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% An individual with knowledge claims lacks institutional affiliation or credentials. They cannot publish in prestige venues, access peer review, secure funding, or establish authority without institutional endorsement. Exit requires abandoning knowledge production or obtaining institutional credentials (high cost, often gatekept by same system). Experiences maximum extraction: investment in knowledge production yields no legitimacy without institutional stamp.
constraint_indexing:constraint_classification(institutional_epistemic_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% A junior researcher within institutions benefits from credential-sharing (publications carry institutional prestige) but also bears asymmetric extraction: long working hours, precarious contracts, publication pressure, citation manipulation incentives. The institution provides both genuine coordination (peer review, equipment access, collaborative infrastructure) AND asymmetric extraction (labor value exceeds compensation, reputation flows to institution not individual). Constrained exit: career capital invested in credentials, high switching costs to alternative epistemic systems.
constraint_indexing:constraint_classification(institutional_epistemic_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Universities, research institutes, professional colleges experience the constraint as coordination: accreditation systems solve the problem of matching credentials to competence, peer review ensures quality, institutional reputation attracts funding and talented researchers. The institution can arbitrage its credentials across jurisdictions and time horizons. Constraint functions as pure coordination from this perspective — enables rather than constrains institutional action.
constraint_indexing:constraint_classification(institutional_epistemic_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized non-institutional actors (open-access advocates, citizen science networks, alternative peer review systems like arXiv overlay journals, blockchain verification) see credentialing as a temporary coordination problem being bypassed. Preprints, open data, decentralized peer review, and reputation systems that don't depend on institutional affiliation are creating parallel epistemic pathways with sunset clauses. Constrained exit because these alternatives are still nascent, but the coalition has agency and sees a clear pathway toward reducing traditional gatekeeping's extraction mechanism.
constraint_indexing:constraint_classification(institutional_epistemic_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The institutional apparatus of accreditation, degree-granting, peer review, journal rank, and citation metrics persists as substantially performative ritual. While some certification function remains (accreditation does screen for basic competence), the primary activity is now theater: impact factors are gamed, peer review is slow and arbitrary, credential inflation is rampant, and institutional prestige masks poor knowledge production. The system maintains itself through inertia and because no institution can unilaterally abandon it, but participants recognize its degraded function. Theater ratio (0.68) reflects this gap.
constraint_indexing:constraint_classification(institutional_epistemic_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Professional associations (medical boards, engineering accreditation, legal bar associations) coordinate genuine public safety and competence maintenance while also extracting monopoly rents through credential scarcity. These bodies provide legitimate gatekeeping (filtering incompetent practitioners) and genuine coordination (standardizing knowledge and practice) alongside asymmetric extraction (restricting labor supply, raising costs, limiting access to services). Constrained exit: breaking professional monopolies requires political action at scale; individual practitioners and public cannot easily circumvent these gates.
constraint_indexing:constraint_classification(institutional_epistemic_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational universal view, some institutional authority is inherent to knowledge production: complex claims require verification, verification requires coordination, coordination requires institutional infrastructure, and institutions require some authority to enforce standards. This perspective risks treating the constraint as a natural law. However, this analysis naturalizes a contingent institutional arrangement — decentralized verification, distributed ledger credentials, and open peer review show that institutional authority concentration is not inherent to knowledge production.
constraint_indexing:constraint_classification(institutional_epistemic_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_epistemic_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_epistemic_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_epistemic_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_epistemic_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_epistemic_authority, TR),
    TR >= 0.70.

:- end_tests(institutional_epistemic_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional epistemic authority captures substantial benefits through credential monopolies: access to publication venues, funding, career advancement, social authority are all gatekept by institutions. However, the extraction is not absolute (0.70+) because genuine coordination functions exist — peer review does catch errors, institutions do enable cumulative knowledge building, credentials do correlate (imperfectly) with competence. The increase from 0.35 to 0.58 over 50 years reflects credential inflation, rent-seeking behavior, and publisher profit maximization outpacing verification function. Suppression (0.65): Moderate-high. Non-institutional knowledge producers face substantial barriers: journal gatekeeping, funding agency bias toward institutions, lack of access to peer review infrastructure, social stigma against non-credentialed claims, career penalties for non-institutional affiliation. However, suppression is not total — open-access infrastructure, arXiv preprints, and citizen science are creating alternative pathways. Theater ratio (0.68): Moderate-high. Institutional verification mechanisms have increasingly become performative: impact factors are gamed through manipulation, peer review is slow (6-18 months), arbitrary (outcomes depend on reviewer personality not claim validity), and unreliable (high false positive and false negative rates). Accreditation ceremonies, degree conferrals, and prestige rankings function as ritual signaling more than verification. The rise from 0.42 to 0.68 reflects credential inflation and institutional prestige concentration, not improvement in verification function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across observer positions. Credentialed institutions classify the constraint as pure coordination (Rope) — they solve the legitimate problem of matching expertise to authority. Excluded producers classify it as pure extraction (Snare) — they bear the costs of institutional gatekeeping with no benefit. Early-career researchers experience genuine hybridity (Tangled Rope) — the system enables their careers while extracting labor value. Professional licensing bodies see mixed coordination-extraction (Tangled Rope) — they coordinate real safety standards alongside monopoly rents. The open knowledge movement sees temporary gatekeeping (Scaffold) — preprints, open peer review, and distributed credentialing are building alternative pathways with sunset logic. The accreditation system sees its own degradation (Piton) — peer review and credentialing persist through inertia, not because they verify competence anymore. The analytical observer risks false natural law (Mountain) — assuming institutional concentration is inherent to knowledge verification rather than a contingent institutional arrangement. The perspectival gaps reveal that the constraint's classification depends entirely on structural position: who benefits, who bears costs, and what exit options are available.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from ~0.05 for credentialed institutions (full beneficiaries with arbitrage exit options) to ~0.95 for powerless excluded producers (full victims with trapped exit options). Credentialed institutions derive low d from institutional power + arbitrage options + beneficiary status — they can move their credentials across jurisdictions and exit constraints easily. Early-career researchers derive moderate-high d from moderate power + constrained exit options + mixed beneficiary-victim status — they benefit from credentials but bear extraction costs. Excluded producers derive maximum d from powerless status + trapped exit options + victim status — they cannot access the system without institutional help, and the system is the only legitimate pathway to authority. Professional licensing bodies derive moderately-high d from organized power + constrained exit options + beneficiary status — they coordinate public safety but extract monopoly rents. The scaffold perspective (open knowledge coalition) derives moderate d from organized power + constrained exit options (alternatives still nascent) + mixed beneficiary-victim status — they benefit from current institutional knowledge infrastructure while suffering extraction from gatekeeping, but see exit pathways emerging. The piton perspective (accreditation system) derives institutional d from arbitrage options but experiences its own degradation — it sees itself as performing theater, maintaining ritual not function.
 *
 * MANDATROPHY ANALYSIS:
 *   INSTITUTIONAL GATEKEEPING RESOLVES THE MANDATROPHY BETWEEN COORDINATION AND EXTRACTION: The constraint cannot be classified as pure coordination (Rope) despite genuine coordination functions because institutional authority concentration extracts substantial rents (credential scarcity, prestige monopoly, labor value differential). Cannot be classified as pure extraction (Snare) because verification functions genuinely exist and benefit the broader epistemic community (quality assurance, error detection, cumulative knowledge building). The Tangled Rope classification captures both: the constraint IS coordination (peer review, quality assurance, cumulative knowledge building) AND asymmetric extraction (credential monopolies, prestige gatekeeping, suppression of alternatives) simultaneously. The mandatrophy resolves by noting that the proportions are shifting: as alternative verification systems mature (open-access journals, arXiv overlay peer review, blockchain credentialing, citizen science networks), the coordination function is being replicated without the extraction overhead. The constraint is trending from tangled rope toward scaffold — institutional authority will become a temporary coordination mechanism rather than a permanent gatekeeping monopoly. The false natural law (analytical mountain perspective) is the biggest diagnostic risk: claiming 'knowledge production requires institutional authority' naturalizes a contingent arrangement that is being actively decomposed by open-science movements. The structural data (theater ratio increasing, extractiveness increasing, alternative systems emerging) contradicts the natural law framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credentialism_versus_competence,
    'Does institutional credentialing correlate with actual knowledge competence, or has the relationship decayed to mere correlation with institutional affiliation?',
    'Cross-validation studies: measure predictive accuracy of credentials against independent competence assessment; compare outcomes for credentialed vs non-credentialed practitioners in same field',
    'If strong correlation: credentialing system provides genuine gatekeeping function (legitimizes extraction as coordination cost). If weak correlation: credentialing is pure rent extraction (reclassifies from tangled rope to snare for credentialed beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credentialism_versus_competence, empirical, 'Whether credentials correlate with actual competence or just institutional affiliation').

omega_variable(
    alternative_verification_sufficiency,
    'Can decentralized peer review (arXiv overlay journals, blockchain-verified reputation), open-source scientific infrastructure, and distributed credentialing systems achieve verification quality comparable to traditional institutional peer review?',
    'Longitudinal comparison of error rates, reproducibility, and citation impact between traditional vs alternative verification pathways; tracking of alternative systems'' maturity and adoption',
    'If comparable: scaffold perspective confirmed — institutional authority is structurally temporary (sunset clause valid). If decentralized systems perform worse: institutional gatekeeping is coordination-necessary, not extraction-exploitative, and classified as rope not tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether alternative verification systems can match institutional quality').

omega_variable(
    knowledge_production_externality_capture,
    'What proportion of institutional epistemic authority serves public good (knowledge accessibility, quality assurance) vs private capture (institutional profit, credential monopoly, reputation concentration)?',
    'Institutional accounting: measure resource allocation between public-benefit activities (teaching, open access) vs rent-extraction activities (prestige journals, credential gatekeeping); compare institutional profit margins before/after open-access transition',
    'If predominantly public good: legitimizes suppression (0.65) as coordination cost. If predominantly private capture: reclassifies as snare for victims; increases victim group to include public.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(knowledge_production_externality_capture, preference, 'Institutional authority capture as public good vs private rent').

omega_variable(
    identity_lock_mechanism_in_credentialism,
    'To what degree are institutional researchers identity-locked (professional identity fused with credentials) versus constrained (high-cost external barriers to exit)?',
    'Interview analysis and career transition study: track researchers who leave institutional systems; measure whether exit involves identity reformation vs material cost reduction',
    'If identity-locked dominates: constraining only professional identity, not career mobility; perspective (moderate/constrained) should be (moderate/identity_locked). If material-constrained dominates: current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_credentialism, empirical, 'Whether credential lock is identity fusion or material barrier').

omega_variable(
    suppression_structural_versus_internalized,
    'Is the suppression of non-institutional knowledge production structural (actual barriers to access, publication, funding) or partly internalized (non-credentialed producers believe their work is illegitimate)?',
    'Comparative analysis: measure suppression before/after removal of explicit barriers (e.g., open-access mandate removing publication gatekeeping); track whether suppression persists post-barrier-removal',
    'If structural dominates: suppression (0.65) is accurate. If internalized dominates: actual suppression is lower but psychological entrapment is higher; identity_locked classification applies to excluded producers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_versus_internalized, empirical, 'Whether suppression is structural or internalized in excluded producers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_epistemic_authority, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iea_tr_t0, institutional_epistemic_authority, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iea_tr_t25, institutional_epistemic_authority, theater_ratio, 25, 0.58).
narrative_ontology:measurement(iea_tr_t50, institutional_epistemic_authority, theater_ratio, 50, 0.68).
narrative_ontology:measurement(iea_tr_t75, institutional_epistemic_authority, theater_ratio, 75, 0.71).

% Extraction over time
narrative_ontology:measurement(iea_be_t0, institutional_epistemic_authority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iea_be_t25, institutional_epistemic_authority, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(iea_be_t50, institutional_epistemic_authority, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(iea_be_t75, institutional_epistemic_authority, base_extractiveness, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_epistemic_authority, information_standard).
narrative_ontology:affects_constraint(institutional_epistemic_authority, credential_inflation).
narrative_ontology:affects_constraint(institutional_epistemic_authority, peer_review_gate_keeping).
narrative_ontology:affects_constraint(institutional_epistemic_authority, open_access_knowledge_systems).

% DUAL FORMULATION NOTE:
% Institutional epistemic authority decomposes into at least three structurally distinct constraints with different epsilon values: (1) peer_review_gate_keeping (ε≈0.48, tangled rope) — the specific gatekeeping mechanism, (2) credential_inflation (ε≈0.62, snare) — the credential scarcity rent mechanism, (3) open_access_knowledge_systems (ε≈0.25, scaffold) — emerging alternative verification systems that bypass institutional gatekeeping. These are linked by network causality: institutional authority (this story) affects all three; credential inflation drives higher extractiveness; open-access systems create sunset clause for institutional gatekeeping.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_epistemic_authority, institutional, 0.1).
constraint_indexing:directionality_override(institutional_epistemic_authority, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
