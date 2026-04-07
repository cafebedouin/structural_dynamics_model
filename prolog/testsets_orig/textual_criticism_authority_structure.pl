% ============================================================================
% CONSTRAINT STORY: textual_criticism_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_textual_criticism_authority_structure, []).

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
 *   constraint_id: textual_criticism_authority_structure
 *   human_readable: Textual Criticism Authority Structure
 *   domain: philology/literary_scholarship/institutional_authority
 *
 * SUMMARY:
 *   The textual criticism authority structure creates a systematic imbalance
 *   between canonical scholars at elite institutions and emerging or
 *   alternative-methodology scholars. The constraint exhibits all six DR
 *   types from different perspectives, revealing how institutional
 *   gatekeeping can be naturalized as epistemological necessity. The same
 *   structural phenomenon — the authority granted to canonical manuscript
 *   traditions and methodologies — appears as an immutable requirement of
 *   rigorous scholarship (mountain), a coordination mechanism enabling shared
 *   standards (rope), a mixed coordination-extraction hybrid with legitimate
 *   knowledge production alongside career gatekeeping (tangled_rope), a pure
 *   extraction mechanism trapping early-career scholars (snare), a temporary
 *   problem being solved by digital humanities and open-access editions
 *   (scaffold), or a degraded ritual maintained by institutional inertia
 *   (piton), depending on the observer's structural position. The
 *   theater_ratio (0.68) reflects that traditional critical apparatus is
 *   substantially performative: readers cannot independently verify
 *   paleographic judgments or genetic-critical reconstructions from apparatus
 *   alone; the elaborate footnote apparatus performs scholarly authority more
 *   than it communicates empirical content. Digital humanities represent an
 *   alternative pathway with genuinely lower theater — computational
 *   collation and open-source editions bypass the performative apparatus
 *   entirely, testing whether distributed analysis can replace canonical
 *   authority.
 *
 * KEY AGENTS:
 *   - Emerging Scholars: Primary victims (powerless/trapped) — gatekeeping enforcement blocks publication and career advancement for dissenting methodologies
 *   - Canonical Institutions: Primary beneficiaries (institutional/arbitrage) — capture funding, prestige, and methodological legitimacy; define what counts as valid scholarship
 *   - Regional Scholarship Communities: Secondary victims (moderate/constrained) — face systematic devaluation of non-canonical language traditions and methodological marginalization
 *   - Alternative Methodologies: Victims (analytical/trapped) — computational approaches, oral-tradition methods, peripheral-language scholarship systematically suppressed regardless of empirical merit
 *   - Digital Humanities Coalition: Organized agents (organized/constrained) — computational text analysis, machine-assisted collation, open-access editions building alternative verification pathways
 *   - Traditional Editorial Apparatus: Institutional actor (institutional/arbitrage) — maintains performative critical apparatus through inertia; recognizes own degradation but lacks replacement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional gatekeeping as inherent requirement of textual rigor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(textual_criticism_authority_structure, 0.52).
domain_priors:suppression_score(textual_criticism_authority_structure, 0.62).
domain_priors:theater_ratio(textual_criticism_authority_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(textual_criticism_authority_structure, extractiveness, 0.52).
narrative_ontology:constraint_metric(textual_criticism_authority_structure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(textual_criticism_authority_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(textual_criticism_authority_structure, tangled_rope).
narrative_ontology:human_readable(textual_criticism_authority_structure, "Textual Criticism Authority Structure").
narrative_ontology:topic_domain(textual_criticism_authority_structure, "philology/literary_scholarship/institutional_authority").

domain_priors:requires_active_enforcement(textual_criticism_authority_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(textual_criticism_authority_structure, established_scholars).
narrative_ontology:constraint_beneficiary(textual_criticism_authority_structure, canonical_institutions).
narrative_ontology:constraint_victim(textual_criticism_authority_structure, emerging_scholars).
narrative_ontology:constraint_victim(textual_criticism_authority_structure, alternative_methodologies).
narrative_ontology:constraint_victim(textual_criticism_authority_structure, peripheral_languages).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING SCHOLAR (SNARE) — Early-career scholars face gatekeeping enforcement: publication requires acceptance by established authorities; dissenting methodological approaches face systematic rejection; no viable path to legitimacy outside the canonical institution network. Trapped by career dependency and resource asymmetry.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL SCHOLARSHIP COMMUNITY (TANGLED ROPE) — Genuine coordination function: shared access to manuscript databases, collaborative editions, peer feedback networks. But also experiences extraction: regional scholarship is systematically devalued; non-canonical language traditions are suppressed; methodological innovation faces resistance. Mixed costs and benefits with significant institutional friction.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CANONICAL INSTITUTION (ROPE) — Benefits from the authority structure through institutional prestige, funding concentration, and methodological legitimacy. Experiences the constraint as pure coordination: shared standards enable collaborative scholarship, resource pooling, and global knowledge exchange. Net beneficiary with genuine arbitrage options.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL HUMANITIES COALITION (SCAFFOLD) — Organized movement (computational text analysis, machine-assisted collation, open-access editions, distributed editorial projects) building alternative verification pathways that bypass traditional peer review gatekeeping. Low effective extraction because coalition members have agency through technology and see sunset logic: as digital tools mature and open-source editions proliferate, the traditional authority bottleneck's gatekeeping power diminishes. Sunset clause: 15-25 years for digital methods to establish methodological parity.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EDITORIAL APPARATUS (PITON) — The critical apparatus (footnotes, variant lists, emendation justifications) is substantially performative. Readers cannot independently verify paleographic judgments or genetic principles from apparatus alone. The elaborate ritual of traditional editing persists through institutional inertia despite diminishing verification capacity. The editorial establishment recognizes its own theater but maintains it because alternatives haven't fully replaced it. Theater ratio dominates classification.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of textual authority concentration is inherent to scholarship: complex texts always require expert adjudication, and the gap between manuscript evidence and interpretive consensus is a structural feature of how textual knowledge advances. This perspective sees the authority structure as an immutable property of philology itself. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to textual science' framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(textual_criticism_authority_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(textual_criticism_authority_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(textual_criticism_authority_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(textual_criticism_authority_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(textual_criticism_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(textual_criticism_authority_structure, TR),
    TR >= 0.70.

:- end_tests(textual_criticism_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Canonical institutions capture career and funding benefits through methodological gatekeeping. The extraction is substantial — early-career scholars face systematic rejection of non-canonical approaches — but not total. Some alternative methodologies publish in parallel outlets, and digital humanities are building competitive pathways. The value reflects real extraction with some escape routes. Suppression (0.62): Moderate-high. Significant barriers to non-canonical scholarship include publication bias against alternative methodologies, career risk of dissenting from canonical judgments, funding concentration in elite institutions, and prestige asymmetries. But suppression is not absolute — some scholars successfully publish alternative work, and digital platforms are lowering publication barriers. Theater ratio (0.68): High. Traditional critical apparatus is substantially performative. The elaborate footnote systems that justify textual choices depend on paleographic judgments readers cannot verify, genetic principles they cannot reconstruct, and editorial principles they cannot reconstruct from apparatus alone. Theater has increased over the 30-year interval as scholars have become more aware of the incommensurability between apparatus presentation and actual verification feasibility. Digital humanities reduce theater by making collation algorithms explicit and testable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — canonical authority in textual scholarship — produces six genuinely different classifications depending on observer position. The canonical institution sees pure coordination (Rope) — shared standards and methodologies enable collaborative knowledge production. The digital humanities coalition sees a temporary problem with a sunset (Scaffold) — computational approaches and open editions are building alternative pathways that will achieve methodological parity. The editorial establishment sees its own degraded apparatus (Piton) — critical apparatus is substantially theater, maintained through inertia, with scholars aware it has lost verification capacity. Regional scholarship communities see mixed coordination and extraction (Tangled Rope) — the system enables collaboration while suppressing non-canonical methodologies. Emerging scholars see pure extraction (Snare) — gatekeeping blocks career advancement regardless of methodological merit. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — some degree of canonical authority is inherent to managing textual complexity — but the structural data reveals this as a false summit: the contingent institutional arrangements (peer review gatekeeping, funding concentration, prestige asymmetries) are not laws of nature, and alternative systems (distributed digital analysis, open-source editions) demonstrate feasibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position within the textual authority flow. Canonical institutions (beneficiaries with arbitrage options) experience low or negative effective extraction — they benefit from the system's authority concentration. Emerging scholars (victims with trapped exit) experience maximum extraction — they bear the full cost of gatekeeping. Regional communities (moderate power with constrained exit) experience mixed extraction — they benefit from shared standards and resources but face suppression of their methodological contributions. Digital humanities (organized power with constrained exit but visible sunset) experience moderate extraction — they have coalition strength and an exit path through technological maturation, reducing experienced chi. Traditional editorial apparatus (institutional beneficiary with arbitrage) experiences negative extraction from its position as gatekeeper, though it also exhibits piton degradation from civilizational perspective. The analytical observer risks false summit by naturalizing what is a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that canonical gatekeeping and legitimate knowledge coordination are genuinely entangled rather than separable. The canonical institution's rope classification is not false — shared standards genuinely enable collaborative scholarship. But the rope is achieved through suppressing alternative pathways (the tangled_rope for regional scholars) and trapping early-career scholars (the snare for emerging scholars). The mandatrophy is not 'is this rope or snare?' but 'at what cost is the rope maintained?' The answer is: the rope's coordination benefits flow primarily to canonical institutions while the suppression costs flow to everyone else. The scaffold perspective (digital humanities with sunset) offers a potential resolution: if alternative methodologies achieve parity without requiring the canonical gatekeeping overhead, then the coordination (shared standards, methodological clarity) can persist while the extraction (gatekeeping, prestige concentration) declines. The piton perspective identifies that traditional apparatus has already lost most of its verification function — it persists through institutional ritual rather than empirical necessity. The false summit (analytical mountain) reveals that naturalization is the gatekeeping mechanism: framing canonical authority as 'how rigorous scholarship must be' rather than 'what our institutional arrangements happened to create.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodology_incommensurability,
    'Are alternative textual methodologies (computational collation, genetic criticism, oral-tradition reconstruction) genuinely incommensurable with canonical philology, or are they suppressed through social gatekeeping?',
    'Blind comparative analysis: identical manuscript problems solved by multiple methodologies; assessment of outcomes without methodological labels; historical tracking of rejected methodologies that later proved productive',
    'If incommensurable: suppression is structurally justified (extraction is coordination overhead). If suppressed by gatekeeping: extraction mechanism is pure (move from tangled_rope toward snare across perspectives). If some are incommensurable and others suppressed: decompose into separate constraint stories per methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodology_incommensurability, empirical, 'Whether suppressed methodologies are incommensurable or gatekept').

omega_variable(
    canonical_bias_magnitude,
    'What proportion of the observed methodological authority imbalance reflects genuine empirical superiority of canonical approaches versus institutional power concentration?',
    'Cross-cultural comparative analysis: textual scholarship in traditions without canonical gatekeeping (oral-formulaic traditions, non-Western manuscripts, digital-native texts); measurement of scholarly productivity and innovation rates by methodological tradition; career outcome analysis controlling for institutional affiliation',
    'If canonical superiority ≥ 70%: extraction is minimal (move toward rope). If institutional power ≥ 60%: extraction is substantial (move toward snare from emerging scholar perspective). If mixed: ratio informs chi calculation for each perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_bias_magnitude, empirical, 'Proportion of authority imbalance due to methodology versus gatekeeping').

omega_variable(
    digital_maturation_timeline,
    'Will computational text analysis and machine-assisted collation achieve methodological parity with canonical philology within 15-25 years, validating the scaffold sunset logic?',
    'Longitudinal tracking of digital methodology adoption across elite institutions; measurement of citation and prestige parity; analysis of training curriculum integration; assessment of publication success rates for digital-first scholarship',
    'If yes: scaffold perspective is structurally sound, extraction is temporary (confirmed sunset). If no: digital humanities remain subordinate, scaffold is aspirational, authority structure persists, reclassify from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_maturation_timeline, empirical, 'Whether digital humanities achieve parity within sunset timeline').

omega_variable(
    apparatus_verification_feasibility,
    'Can readers realistically reconstruct paleographic judgments and genetic principles from traditional critical apparatus, or is the apparatus inherently non-verifiable theater?',
    'Experimental reconstruction: provide apparatus to trained paleographers without original manuscripts; measure reconstruction accuracy; assess whether apparatus communicates genuine empirical content or merely ritualistic scholarly posturing',
    'If verifiable: theater_ratio should be lower, piton classification weakens. If inherently theatrical: theater_ratio confirmed, piton classification strengthened, identifies a fundamental asymmetry in textual authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apparatus_verification_feasibility, empirical, 'Whether critical apparatus is verifiable or theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(textual_criticism_authority_structure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tc_auth_tr_t0, textual_criticism_authority_structure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(tc_auth_tr_t15, textual_criticism_authority_structure, theater_ratio, 15, 0.58).
narrative_ontology:measurement(tc_auth_tr_t30, textual_criticism_authority_structure, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(tc_auth_be_t0, textual_criticism_authority_structure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tc_auth_be_t15, textual_criticism_authority_structure, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(tc_auth_be_t30, textual_criticism_authority_structure, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(textual_criticism_authority_structure, information_standard).
narrative_ontology:affects_constraint(textual_criticism_authority_structure, peer_review_gatekeeping).
narrative_ontology:affects_constraint(textual_criticism_authority_structure, manuscript_tradition_canonicalization).
narrative_ontology:affects_constraint(textual_criticism_authority_structure, academic_prestige_concentration).

% DUAL FORMULATION NOTE:
% The textual criticism authority structure is downstream of specific scholarly traditions and upstream of individual publishing outcomes. Decomposition into separate stories would yield: canonical_methodology_legitimacy (ε≈0.08, Mountain), alternative_methodology_suppression (ε≈0.68, Snare), and digital_humanities_transition (ε≈0.35, Scaffold). This story captures the aggregate structural dynamic across all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(textual_criticism_authority_structure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
