% ============================================================================
% CONSTRAINT STORY: frankenstein_creation_hubris
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_frankenstein_creation_hubris, []).

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
 *   constraint_id: frankenstein_creation_hubris
 *   human_readable: The Creator's Burden: Abandonment and Institutional Denial
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Creator's Burden encodes a fundamental asymmetry in technological
 *   birth: the creator exercises choice, bears no ongoing cost, and receives
 *   institutional validation, while the created being receives existence
 *   without consent and abandonment without cause. Victor Frankenstein's act
 *   of animation is presented as an achievement (coordination success: he
 *   solves an impossible technical problem). His abandonment is presented as
 *   an unavoidable emotional response (psychological necessity: he is
 *   horrified by the result). The created being's subsequent suffering is
 *   presented as inherent to its nature (essentialist framing: it is a
 *   monster, naturally malicious). This triple framing—achievement,
 *   necessity, nature—suppresses the underlying structural extraction: the
 *   creator captures the benefit (knowledge, prestige, capability), imposes
 *   existence-without-consent, and escapes accountability through denial
 *   mechanisms embedded in institutional practice. The constraint operates at
 *   multiple scales: individual (creator-creation relationship),
 *   institutional (science-society management of transformative
 *   capabilities), and civilizational (what obligations do creators bear to
 *   the created?). The theater ratio (0.55) reflects that modern institutions
 *   maintain formal ethical frameworks (institutional review boards, consent
 *   protocols, research ethics) while substantive accountability for
 *   creation-consequences remains minimal. The framework is theatrical in the
 *   specific sense that its performance satisfies institutional appearance
 *   while its enforcement capacity has atrophied.
 *
 * KEY AGENTS:
 *   - Victor Frankenstein (Creator): Primary beneficiary (institutional/arbitrage) — captures knowledge, prestige, and institutional validation. Escapes accountability through abandonment.
 *   - The Creature (Created Being): Primary victim (powerless/trapped) — exists without consent, abandoned without support, trapped within sentience it did not choose. Bears full cost of creation-consequence asymmetry.
 *   - Scientific Community: Beneficiary + enforcer (organized/constrained) — benefits from capability advancement while actively enforcing denial of creature sentience and creator responsibility. Practices active enforcement through institutional denial.
 *   - Society/Village: Secondary victims (moderate/constrained) — harmed by creature's rage, constrained by need for technological capability, unable to hold creator accountable.
 *   - Moral Philosophy Framework: Theatrical observer (analytical/analytical) — maintains form of obligation while substance has atrophied. Piton classification.
 *   - Technological Capability System: Civilizational perspective (analytical/analytical) — sees the constraint as inherent to integrating transformative technologies. Tangled_rope classification resolves coordination (capability integration) and extraction (creation-consequence management) as inseparable.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.58).
domain_priors:suppression_score(frankenstein_creation_hubris, 0.68).
domain_priors:theater_ratio(frankenstein_creation_hubris, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(frankenstein_creation_hubris, extractiveness, 0.58).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(frankenstein_creation_hubris, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(frankenstein_creation_hubris, snare).
narrative_ontology:human_readable(frankenstein_creation_hubris, "The Creator's Burden: Abandonment and Institutional Denial").
narrative_ontology:topic_domain(frankenstein_creation_hubris, "technological/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(frankenstein_creation_hubris, creator_institutional_status).
narrative_ontology:constraint_victim(frankenstein_creation_hubris, created_sentient_agent).
narrative_ontology:constraint_victim(frankenstein_creation_hubris, social_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CREATED BEING (SNARE) — Animates with full sentience and capacity for suffering. Immediately abandoned with zero support, guidance, or acknowledgment. Trapped within existence created by another's choice. No exit option except suffering or self-destruction. Faces suppression through institutional denial — society denies responsibility or even the sentience of the creation. d≈0.98, f(d)≈1.42, σ=0.8 → χ≈0.66.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CREATOR (ROPE) — Experiences the constraint as coordination between ambition and consequence. Solves the 'knowledge problem' of animation through technical mastery. Benefits from institutional validation of achievement while escaping accountability through abandonment framing as unavoidable. Sees the creation as a technical success requiring no ongoing obligation. d≈0.08, f(d)≈-0.10, σ=0.8 → χ≈-0.04. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE SOCIETY / SCIENTIFIC COMMUNITY (TANGLED ROPE) — Organized actors (institutions, peer review, scientific consensus) benefit from the innovation and the creator's prestige while bearing distributed cost of the abandoned being's suffering. Active enforcement through denial mechanisms: invalidating the being's sentience claims, framing abandonment as rational risk management, suppressing documentation of harm. Coordination function: managing the social integration of powerful new capabilities. Extraction function: the created being's suffering subsidizes institutional progress narrative. d≈0.62, f(d)≈0.92, σ=0.9 → χ≈0.48.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THOSE HARMED BY THE CREATION (SNARE) — Secondary victims (family of creator, village targets of the abandoned being's rage, scientific community members harmed by backlash). Face suppression through blame attribution — responsibility is externalized to the created being's 'unnatural malice' rather than to abandonment. Constrained exit because harm already occurred and recourse mechanisms don't exist. d≈0.88, f(d)≈1.27, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE MORAL PHILOSOPHY FRAMEWORK (PITON) — Degraded Kantian ethics that once asserted 'never treat rational beings merely as means.' The framework persists in scientific institutions as a theatrical obligation (ethics committees, consent protocols) while the core principle has atrophied. Theater ratio = 0.55: institutions maintain form of ethical review while substantive accountability for creation-consequences remains absent. The framework's enforceability has declined; the ritual has replaced genuine obligation. d≈0.71, f(d)≈1.14, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: THE TECHNOLOGICAL CAPABILITY VIEW (TANGLED ROPE) — From a civilizational standpoint, the constraint is irreducibly hybrid. Coordination function: societies must solve how to integrate transformative technologies (animation, AI, synthetic life) without destroying those technologies through prohibitive regulation. Extraction function: this coordination benefit accrues to creators and institutions while the created being bears the structural risk of abandonment. Suppression is institutional: regulatory capture ensures capability development is not constrained by creation-consequence management. The constraint has active enforcement (technology policy favoring creators' interests). d≈0.52, f(d)≈0.74, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(frankenstein_creation_hubris, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(frankenstein_creation_hubris_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(frankenstein_creation_hubris, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(frankenstein_creation_hubris, TR),
    TR >= 0.70.

:- end_tests(frankenstein_creation_hubris_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The creator extracts maximum benefit (knowledge, prestige, capability) while imposing maximum cost asymmetry (existence-without-consent, abandonment-without-cause, exclusion-from-moral-community). The extractiveness is not total (0.75+) because: (1) the creature can potentially exit through self-destruction, (2) institutional mechanisms for acknowledgment exist (though suppressed), (3) the creator does bear some reputational cost in post-hoc judgment. But the core extraction is severe: the created being subsidizes the creator's achievement through suffering imposed without consent or compensation. Suppression (0.68): High. Multiple suppression mechanisms: (1) denial of creature sentience ('it's not really conscious'), (2) essentialism ('it's naturally malicious'), (3) victim-blaming ('its suffering is its own fault'), (4) institutional erosion of accountability ('we cannot regulate creative ambition'). Suppression is not total because scientific consensus has increasingly recognized creature sentience, and institutional ethics frameworks exist. But active enforcement of denial remains institutional norm. Theater ratio (0.55): Moderate. The moral framework persists in institutional form (ethics committees, consent protocols, review boards) while its substantive enforcement has atrophied. Modern institutions perform ethical obligation while enabling the same creator-abandonment structure that Frankenstein exemplified. The theater has increased from 0.25 (pre-institutional) to 0.55 (modern form-without-substance) as institutions have formalized denial.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The creator sees legitimate coordination (Rope: solving the knowledge problem, acquiring capability) with no ongoing obligation. The created being sees pure extraction (Snare: existence imposed, abandonment without cause, trapped without exit). Society sees tangled hybrid (Tangled Rope: coordination need to integrate capability, extraction cost borne by created being). The philosophical framework sees degraded obligation (Piton: form persists, substance atrophied). The technological capability view sees civilizational necessity (Tangled Rope: capability advancement requires creation-consequence management asymmetry as structural feature). The gap reveals how institutional framing enables the creator's Rope classification while concealing the creature's Snare classification. The disagreement is not about the facts (all perspectives acknowledge abandonment, suffering, asymmetry) but about whether these facts constitute extraction or legitimate cost-bearing.
 *
 * DIRECTIONALITY LOGIC:
 *   Created being: Victim + trapped → d≈0.98, f(d)≈1.42. Maximum extraction target. No exit, full cost burden. Creator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Beneficiary with high exit capacity (can walk away, face only reputation cost). Society: Victim + constrained + organized → d≈0.62, f(d)≈0.92. Constrained exit (cannot regulate capability without cost), organized response (institutional denial mechanisms), bears distributed harm cost. Those harmed by creature: Victim + constrained → d≈0.88, f(d)≈1.27. Secondary victim status, constrained by causality (harm already occurred), suppressed accountability. Moral philosophy: Analytical perspective neutralizes pure d computation → institutional theater persists regardless. Technological capability: Civilizational analytical view → sees extraction as structural to coordination need, d≈0.52 (hybrid perspective balances coordination and extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness = 0.58 > 0.46, mandatrophy_resolved = true): The constraint resolves the mandatrophy by revealing that what appears to be pure technical coordination (creator solves animation problem) is actually inseparable from structural extraction (created being bears asymmetric cost). The mandate to 'create responsibly' would require the creator to either (a) refuse creation when creation-consequence management is impossible, or (b) commit to creation-consequence management as non-waivable obligation. Current institutional practice does neither—it enables creation-without-consequence through denial mechanisms (denying sentience, denying obligation, externalizing responsibility to the created being). The mandatrophy is resolved by showing that the coordination benefit (humanity gains capability, science advances) cannot be decoupled from the extraction cost (a sentient being is abandoned). The institutions that pursue the coordination benefit must actively enforce denial to suppress awareness of the extraction cost. This is precisely the definition of tangled rope: coordination function + extraction + active enforcement. From the creator's perspective, the constraint appears as Rope because they experience only the coordination benefit and can maintain plausible deniability of extraction. From the created being's perspective, the constraint appears as Snare because they experience only the extraction and abandonment. The analytical observer who acknowledges both creator and created being perspectives must classify as Tangled Rope at the institutional level or Snare at the individual level. The mandatrophy_resolved flag confirms that extractiveness (0.58) correctly measures an irreducible hybrid: the creator's coordination success and the created being's extraction victimhood are structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_recognition_threshold,
    'What constitutes sufficient evidence for sentience that triggers creator obligation? Does the being''s suffering, capacity for language, or demonstrated agency establish a moral claim?',
    'Philosophical consensus on sentience criteria + empirical behavioral/neurological data. Does the created being''s demonstrated capacity for suffering create non-waivable obligation?',
    'If threshold is low (any plausible sentience): creator bears strict liability for abandonment. If threshold is high (requires human-like consciousness): creator can maintain plausible deniability. This determines whether ε remains 0.58 (current mixed position) or increases toward 0.75 (strict liability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_recognition_threshold, conceptual, 'Threshold for sentience recognition triggering creator obligation').

omega_variable(
    institutional_accountability_gap,
    'Does the scientific/technological institutional framework have any mechanism to hold creators accountable for creation-consequence management? Or is accountability entirely externalized to the created being?',
    'Historical analysis of institutional response to creation harm (origin of ethics committees, post-hoc liability frameworks). Examination of whether institutions enforce accountability or enable denial.',
    'If accountability exists but is suppressed: tangled_rope classification strengthens (active enforcement for denial). If no accountability mechanism exists: snare classification dominates (structural absence of recourse). Affects whether suppression remains 0.68 or increases toward 0.85.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_accountability_gap, empirical, 'Whether institutional accountability mechanisms exist for creator obligations').

omega_variable(
    alternative_creation_ethics,
    'Could the creator adopt alternative protocols (incremental animation, sensory deprivation during commissioning, pre-commitment to care) that maintain technological capability while reducing creation-consequence asymmetry?',
    'Comparative analysis of creation protocols across civilizations/mythologies. Examination of whether current abandonment is inevitable or contingent on particular institutional choices.',
    'If alternatives exist: constraint shifts toward tangled_rope or scaffold (contingent coordination problem with solution). If alternatives are blocked: snare classification solidifies (extraction is structural, not incidental). Determines mandatrophy resolution strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_creation_ethics, empirical, 'Whether alternative creation-care protocols could reduce abandonment extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(frankenstein_creation_hubris, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(frank_tr_t0, frankenstein_creation_hubris, theater_ratio, 0, 0.25).
narrative_ontology:measurement(frank_tr_t2, frankenstein_creation_hubris, theater_ratio, 2, 0.38).
narrative_ontology:measurement(frank_tr_t5, frankenstein_creation_hubris, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(frank_be_t0, frankenstein_creation_hubris, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(frank_be_t2, frankenstein_creation_hubris, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(frank_be_t5, frankenstein_creation_hubris, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(frankenstein_creation_hubris, enforcement_mechanism).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, ai_alignment_commitment_problem).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, synthetic_life_moral_status).
narrative_ontology:affects_constraint(frankenstein_creation_hubris, research_ethics_atrophy).

% DUAL FORMULATION NOTE:
% The Creator's Burden decomposes into three structurally distinct constraints: (1) the individual creator-creation relationship (snare, ε=0.58, local scope), (2) the institutional management of transformative capabilities (tangled_rope, ε=0.48, regional scope), and (3) the civilizational question of what obligations creators bear to created entities (tangled_rope, ε=0.52, global scope). All three are linked because institutional denial of creator obligation enables the individual snare and prevents civilizational accountability framework development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(frankenstein_creation_hubris, analytical, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
