% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic-Script Orthographic Regime of Ottoman Turkish (Continuity Reading)
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Ottoman Arabic-script orthographic regime — Turkish written in a
 *   modified Arabic script, maintained by the chancery, the medreses, and the
 *   calligraphic and scribal traditions — stood as the sole legitimate
 *   orthography for roughly six centuries and entered open contest in the
 *   nineteenth century. This story instantiates the continuity_reading of the
 *   orthographic_kernel: the reading held by the script's custodians, for
 *   whom the arrangement's point is that it carries Ottoman cultural
 *   continuity and direct access to the Islamic textual corpus across
 *   generations. The epsilon referent is the standing arrangement itself,
 *   assessed by this reading's own lights: the reading does not deny the
 *   arrangement's costs — it names them as the custodial burden its own
 *   constituency bears — and it assesses the blocked reform path as a low
 *   cost to state modernization, which is precisely what its siblings deny.
 *   Per the epsilon-invariance rule the three readings are separate stories
 *   linked by network.affects_constraints; this file authors only the
 *   continuity reading. Claim and metrics are authored independently: the
 *   claimed type states this reading's structural verdict — a genuine
 *   continuity function operating through asymmetric custodial extraction
 *   under active enforcement — while the metrics state what the arrangement's
 *   operation descriptively shows.
 *
 * KEY AGENTS:
 *   - ottoman_literate_class: primary target (organized/identity_locked) — ulema, scribes, calligraphers, poets; bears the custodial burden the continuity function runs on
 *   - ottoman_state: agenda-setter and beneficiary (institutional/constrained) — administers the orthography and draws caliphal legitimacy from it
 *   - modernizing_state_bureaucrats: beneficiary-side seat with real frictions (institutional/constrained) — rides the arrangement's legitimacy while pressing a blocked reform path
 *   - islamic_textual_community: diffuse beneficiary (moderate/identity_locked) — trans-regional readers whose corpus access the shared script provides
 *   - turkish_speaking_masses: excluded (powerless/trapped) — the illiterate majority outside the conversation that decides the script
 *   - ottomanist_historians: analytical observer — reads the whole structure from outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.52).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic-Script Orthographic Regime of Ottoman Turkish (Continuity Reading)").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '29b8fad8-fff5-4457-9f5a-fe606a4fd2bb').
narrative_ontology:cs_kernel_codification('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', formalized).
narrative_ontology:cs_authority_grounding('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', lineage).
narrative_ontology:cs_interpretation_layer_present('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb').
narrative_ontology:cs_reading_relation('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', foundational, script_continuity_outweighs_reform_gains).
narrative_ontology:cs_axiom_status(script_continuity_outweighs_reform_gains, holdable).
narrative_ontology:cs_axiom_grounding('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', script_continuity_outweighs_reform_gains, deontological).
narrative_ontology:cs_axiom('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', foundational, orthography_carries_civilizational_memory).
narrative_ontology:cs_axiom_status(orthography_carries_civilizational_memory, holdable).
narrative_ontology:cs_axiom_grounding('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', orthography_carries_civilizational_memory, empirically_contingent).
narrative_ontology:cs_reference_frame('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', quranic_transmission_lineage).
narrative_ontology:cs_drift_state('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', late_ottoman_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29b8fad8-fff5-4457-9f5a-fe606a4fd2bb', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_state).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, modernizing_state_bureaucrats).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, islamic_textual_community).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, modernizing_state_bureaucrats).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, transmission_lineage_doctrine).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, continuity_supremacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ulema, chancery scribes, calligraphers, and divan poets trained over a decade or more in an orthography whose etymological spellings and sparse vowel marking fit Turkish poorly. They copy, teach, and adjudicate the corpus; the tradition runs through their hands. Their skills, patronage, and standing are all denominated in this script, so leaving it means unmaking themselves — and when the state changed the script in 1928, a lifetime of capital became unreadable overnight. They also collect standing, office, and authority from being the ones who can read what others cannot.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class, payer,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ottoman_literate_class, beneficiary).

% The dynasty, the Grand Vizierate, and the chancery administer the orthography: official documents, the school curriculum, and the printing regulations all run through the script the state maintains. Continuity with the Islamic textual tradition is a pillar of the caliphal legitimacy the state claims, so the state both enforces the arrangement and draws standing from it. Its modernizing wings press for orthographic reform from the 1860s onward and are held off inside the state's own councils.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_state, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, ottoman_state, beneficiary).

% Tanzimat and Unionist administrators building codified law, railways, telegraph lines, and state schools inside the inherited arrangement. They ride the legitimacy and administrative continuity the script regime provides — the archive, the high register, the caliphal frame — while protesting the friction it puts on mass schooling, printing, and technical translation. From the 1860s they propose phonetic and Latin-script reforms; every proposal stalls in council. They finally hold the pen that ends the arrangement in 1928.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, modernizing_state_bureaucrats, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, modernizing_state_bureaucrats, payer).

% The trans-regional community of readers from the Balkans to the Hijaz whose shared script gives direct access to Arabic, Persian, and Ottoman texts without translation or transliteration. Access to the Qur'an and the scholarly corpus in the original script is constitutive of their practice. They hold no seat in the chancery councils where orthography is decided, but the arrangement's continuity function exists for them.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, islamic_textual_community, beneficiary,
    moderate, civilizational, identity_locked, continental).

% The largely illiterate Turkish-speaking majority. The orthography's mismatch with spoken Turkish — eight vowels carried by three marks, spellings fixed by Arabic and Persian etymology — makes literacy a years-long attainment available mostly to those with leisure and tutoring. They are the subject of every reform argument and present at none of them; the contest over the script is conducted entirely among people who can already read.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, turkish_speaking_masses, excluded,
    powerless, biographical, trapped, continental).

% Scholars working after the fact in both scripts, reading the archive and the reform record side by side. They can see the whole structure — what the arrangement carried, what it cost, who argued what — from no seat inside it.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottomanist_historians, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, ottoman_state).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single standardized high register connecting six centuries of state records, divan literature, and religious scholarship; gives the Persianate-Arabicate reading community direct access to the Qur'an and the scholarly corpus without translation or transliteration; and carries the chancery's administrative memory across generations and succession crises.
% TRANSFER_FUNCTION: Moves training-years, reproduction labor, and textual custody from the literate class — and literacy access from the Turkish-speaking majority — into the production of continuity, which accrues as caliphal legitimacy to the state and as corpus access to the trans-regional textual community.
% ABSENT_VOICES: The Turkish-speaking masses, for whom the orthography's mismatch with spoken Turkish priced literacy beyond reach; and the vernacular writers and would-be popular publishers whose registers the high orthography excluded. They are absent because the script contest was conducted entirely among people who could already read. The excluded seat is commentary-grade and drives no classification override.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the chancery's memory becomes unreadable, the medrese curriculum loses its substrate, the trans-regional corpus access breaks, and the state loses a pillar of its caliphal legitimacy — the arrangements of every literate institution in the empire depend on it, which is exactly why ending it took a revolutionary state in 1928 rather than drift.
% FOUNDING_PROBLEM: An empire administering a multi-lingual, multi-confessional domain needed a unified high register and a script binding its administration and high culture to the Islamic textual tradition and the chancery's institutional memory; the Arabic script, adapted to Turkish from the fourteenth century, was that solution.
% FOUNDING_PROBLEM_CORROBORATION: Comparative manuscript scholarship and Turkology — orientalist and Turkish, outside every beneficiary seat — attest both that the script solved the trans-generational transmission problem and that the continuity it carried is real. Post-reform Republican testimony corroborates from the opposing camp: the reformers' own acknowledgment that the 1928 change severed new generations from the Ottoman archive is an admission that the old arrangement was carrying what this reading says it carried. The excluded masses, never seated, could not corroborate from inside; their silence is the absence the absent_voices field records.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the arrangement runs on conscripted custodial labor: a decade or more of training per literate, reproduction and adjudication of the corpus, all under an orthography that fits Turkish poorly — and the burden grew as the corpus grew and the opportunity cost of script fidelity rose against European print modernity. Suppression (0.52) is moderate and structural rather than interpersonal: chancery norms, the medrese curriculum, and printing regulation held rival orthographies off; the scalar sits at the interval end, where wartime state collapse had already thinned enforcement. Suppression is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Theater (0.45) records the arrangement's late-interval drift: as functional defense of the script gave way before print and vernacular pressure, its defense migrated to the symbolic register — calligraphy as court art, continuity as legitimation — a Goodhart drift visible in the rising theater series. Accessibility collapse is low (0.40) because the alternatives never vanished: phonetic and Latin-script proposals were argued openly from Münif Pasha's circle in the 1860s through Enver Pasha's Enveriyye in 1908. Resistance is high (0.58) for the same reason — the arrangement fell to a reform coalition, not to neglect. The measurement series share one grid (t maps to approximately 1839 + t; t=90 is the eve of the November 1928 abolition); extractiveness and theater rise monotonically, while the suppression series rises under contestation and sags as the enforcing state's capacity collapsed through the wars of 1912-1922 — the terminal collapse is the abolition itself, at the series' edge of resolution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats compute differently from the same arrangement. From the literate class's position the arrangement is a custodial conscription: it takes their training years, their labor, and finally their capital's legibility, and pays them in standing they cannot spend anywhere else. From the state's position it is the legitimacy vehicle it administers; from the modernizing bureaucrats' position it is the frame their project rides and the friction they protest. The excluded masses — whom this reading does not seat as victims — would compute a fourth position entirely: an arrangement that priced literacy beyond their reach; that position belongs to the modernization_reading's story, not this one. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The victim declaration (ottoman_literate_class, identity_locked) drives the literate seat to the target end — identity lock is what makes the custodial burden inescapable: the class's skills, patronage, and self-understanding are all denominated in the script. The beneficiary declarations drive the state and the modernization seats to the beneficiary end: the arrangement subsidizes the state's caliphal legitimacy and, in this reading's assessment, costs the modernization project little — the blocked reform path is the modernizers' complaint, which this reading discounts on the Tanzimat record (law codes, railways, telegraph, state schools, and a printed press all arrived inside the script). One override is declared: both institutional seats (the state, the modernizing bureaucrats) are set to d = 0.24 because their dual positions — the state is agenda-setter and beneficiary; the modernizers are beneficiary and protesting payer — would leave the derivation mid-scale, and this reading's structural verdict is that both sit beneficiary-side. The islamic_textual_community sits near the beneficiary end on a civilizational horizon. Coalition check: the literate class was genuinely organized (guilds, medrese networks, chancery corporate memory) — its coalition potential was real, but identity lock blunted exit rather than coordination, so organized power here raises the cost it can bear, not its ability to leave.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification keeps two mislabelings apart. Against the siblings' temptation, it keeps the genuine coordination function visible: the arrangement really did transmit a corpus across six centuries and really did give a trans-regional community direct textual access, so reading it as pure extraction erases the function its custodians actually performed. Against the traditionalist temptation, it keeps the extraction visible: the continuity was bought with conscripted custodial labor under an orthography that priced literacy beyond the majority, so reading it as pure coordination launders the burden. The R5 interview records a founding problem (unify administration and bind it to the Islamic textual corpus) still live at interval end, so the mismatch flag does not fire: what ended was not the function but the arrangement, overthrown while its function was live. Mandatrophy here is not a dead mandate carried by habit; it is a live mandate carried by an arrangement whose extraction had made it indefensible to the successor coalition that inherited the state's pen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the continuity_reading of the orthographic_kernel; the modernization_reading and rupture_reading instantiate different constraints over the same historical arrangement. Which reading''s victim set and epsilon distribution should govern classification of the shared arrangement?',
    'Author all three sibling stories and compare the engine''s per-seat classifications across the family; the disagreement locates in the victim set (literate custodians versus excluded masses versus the severed past) and in the epsilon assessed for the modernization path.',
    'Under the modernization reading the victim set shifts to the Turkish-speaking masses and the blocked reform path, and the state seats'' effective extraction rises; under the rupture reading the arrangement is assessed as what the 1928 change rightly severed. Every seat''s classification moves with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed structure: one standing arrangement, three readings, three epsilon distributions.').

omega_variable(
    custodial_burden_vs_monopoly_rent,
    'Is the literate class''s burden genuine custodial conscription, or is it offset — or outweighed — by the office, authority, and patronage the class collected from control of script literacy?',
    'Net-position analysis of ilmiye and chancery careers: training years and reproduction labor weighed against salaries, endowments, and textual authority; comparison with the class''s fortunes after 1928, when its capital became unreadable overnight.',
    'If rents dominate, the literate class is net beneficiary-side, the victim set thins toward the excluded masses, and the arrangement reads as captured rather than custodial; if the burden dominates, the authored high-epsilon victim set stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_burden_vs_monopoly_rent, empirical, 'Whether the victim set''s burden is net cost or rent-offset.').

omega_variable(
    blocked_reform_path_cost,
    'What did the blocked reform path actually cost state modernization — this reading assesses the cost as low; the modernization_reading assesses it as the decisive bottleneck?',
    'Comparative literacy and administrative-capacity trajectories: Ottoman Turkey against Egypt and Iran (script retained longer) and against post-1928 Turkey (script changed), controlling for war and fiscal capacity.',
    'A high counterfactual cost would contradict this reading''s own low-epsilon assessment for the modernization seat and pull the story toward the modernization_reading''s structure; a low cost would vindicate this reading''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocked_reform_path_cost, empirical, 'Counterfactual cost of the blocked reform path to state modernization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_continuity_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(orthographic_continuity_tr_t15, orthographic_kernel__continuity_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(orthographic_continuity_tr_t30, orthographic_kernel__continuity_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(orthographic_continuity_tr_t45, orthographic_kernel__continuity_reading, theater_ratio, 45, 0.29).
narrative_ontology:measurement(orthographic_continuity_tr_t60, orthographic_kernel__continuity_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(orthographic_continuity_tr_t75, orthographic_kernel__continuity_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(orthographic_continuity_tr_t90, orthographic_kernel__continuity_reading, theater_ratio, 90, 0.45).

% Extraction over time
narrative_ontology:measurement(orthographic_continuity_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(orthographic_continuity_be_t15, orthographic_kernel__continuity_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(orthographic_continuity_be_t30, orthographic_kernel__continuity_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(orthographic_continuity_be_t45, orthographic_kernel__continuity_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement(orthographic_continuity_be_t60, orthographic_kernel__continuity_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement(orthographic_continuity_be_t75, orthographic_kernel__continuity_reading, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(orthographic_continuity_be_t90, orthographic_kernel__continuity_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_continuity_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(orthographic_continuity_su_t15, orthographic_kernel__continuity_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(orthographic_continuity_su_t30, orthographic_kernel__continuity_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(orthographic_continuity_su_t45, orthographic_kernel__continuity_reading, suppression_requirement, 45, 0.63).
narrative_ontology:measurement(orthographic_continuity_su_t60, orthographic_kernel__continuity_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(orthographic_continuity_su_t75, orthographic_kernel__continuity_reading, suppression_requirement, 75, 0.54).
narrative_ontology:measurement(orthographic_continuity_su_t90, orthographic_kernel__continuity_reading, suppression_requirement, 90, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Arabic script question' covers three structurally distinct readings of one kernel. This file authors the continuity_reading only: the standing arrangement (Ottoman Turkish in Arabic script) assessed by its custodians' lights, yielding high epsilon for the literate class as victim set and low epsilon for the blocked reform path. The modernization_reading authors a different distribution over the same referent (the excluded masses and the modernization path as victims, the state seats as targets); the rupture_reading authors the arrangement as what the 1928 change rightly severed. The three are separate stories linked by network.affects_constraints; epsilon stays invariant within each file. The upstream/downstream structure runs through the rupture reading's retrospective corroboration: the reformers' own admission that the change severed archival access is evidence the continuity reading's referent was carrying what it claimed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_kernel__continuity_reading, institutional, 0.24).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
