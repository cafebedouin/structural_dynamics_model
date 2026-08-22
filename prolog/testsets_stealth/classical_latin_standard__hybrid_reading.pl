% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__hybrid_reading, []).

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
 *   constraint_id: classical_latin_standard__hybrid_reading
 *   human_readable: Hybrid Reading of the Classical Latin Standard
 *   domain: historical linguistics/philology/commitment systems
 *
 * SUMMARY:
 *   A contested correctness regime governing written Latin from the humanist
 *   turn to the present. The colloquial label 'correct Latin' covers three
 *   structurally distinct standards; this file instantiates the hybrid
 *   reading: fidelity to the Classical canon binds general prose, while
 *   technical and ecclesiastical registers are licensed to develop — coinages
 *   ratified through recognized channels, unsanctioned drift marked as
 *   barbarism. The arrangement solved a real supranational communication
 *   problem for the Church and the respublica litterarum, and simultaneously
 *   routed linguistic authority toward metropolitan adjudicating centers
 *   whose verdicts carried occupational consequences for chancery clerks,
 *   provincial scholars, and schoolmasters formed in older practice.
 *   Enforcement peaked in the sixteenth-century college system, decayed as
 *   vernaculars absorbed learned life, and survives in intensified niches —
 *   canon law, biological nomenclature, Vatican drafting, Neo-Latin
 *   communities — where the standard is voluntarily inhabited and
 *   identity-fused. Claim and metrics are authored independently: the claimed
 *   type states the structure I believe true; the metrics describe the
 *   regime's operation as I read the record. KEY AGENTS (by structural
 *   relationship): - roman_curia_officials: agenda-setting beneficiary
 *   (institutional/constrained) — administers the official register and
 *   collects a neutral supranational instrument - classical_philologists:
 *   adjudicating beneficiary with agenda-setting duties
 *   (organized/identity_locked) — occupational rents flow from the scarcity
 *   their standard creates - latin_school_educators: enforcing beneficiary
 *   (institutional/constrained) — ran the colleges where the standard was
 *   instilled - medieval_trained_notaries: primary payer
 *   (moderate/constrained) — trained usage relabeled barbarous; retraining or
 *   ridicule were the priced options - provincial_humanist_scholars: payer
 *   (moderate/constrained) — usage marked provincial by arbiters they could
 *   not answer - scientific_nomenclature_bodies: beneficiary
 *   (organized/mobile) — inherits the naming medium, coins under the licensed
 *   clause, exit open - vernacular_advocates: excluded (powerful/mobile) —
 *   objected from outside the tribunals; ultimately exited the arrangement
 *   entirely - neolatin_community_members: payer and incidental beneficiary
 *   (moderate/identity_locked) — voluntary composers absorbing correction
 *   while enjoying the coinage license - philological_historians: analytical
 *   observer (analytical/analytical) — sees the whole adjudication structure
 *   from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, 0.48).
domain_priors:suppression_score(classical_latin_standard__hybrid_reading, 0.4).
domain_priors:theater_ratio(classical_latin_standard__hybrid_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__hybrid_reading, "Hybrid Reading of the Classical Latin Standard").
narrative_ontology:topic_domain(classical_latin_standard__hybrid_reading, "historical linguistics/philology/commitment systems").

domain_priors:requires_active_enforcement(classical_latin_standard__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__hybrid_reading, '47a555b5-4e78-40a6-93ea-186ea7a424e0').
narrative_ontology:cs_kernel_codification('47a555b5-4e78-40a6-93ea-186ea7a424e0', fixed_text).
narrative_ontology:cs_authority_grounding('47a555b5-4e78-40a6-93ea-186ea7a424e0', lineage).
narrative_ontology:cs_interpretation_layer_present('47a555b5-4e78-40a6-93ea-186ea7a424e0').
narrative_ontology:cs_reading_relation('47a555b5-4e78-40a6-93ea-186ea7a424e0', classical_latin_standard__continuity_reading, influences).
narrative_ontology:cs_reading_relation('47a555b5-4e78-40a6-93ea-186ea7a424e0', classical_latin_standard__reconstruction_reading, coexists_with).
narrative_ontology:cs_axiom('47a555b5-4e78-40a6-93ea-186ea7a424e0', foundational, classical_norms_bind_outside_sanctioned_domains).
narrative_ontology:cs_axiom_status(classical_norms_bind_outside_sanctioned_domains, holdable).
narrative_ontology:cs_axiom_grounding('47a555b5-4e78-40a6-93ea-186ea7a424e0', classical_norms_bind_outside_sanctioned_domains, conventional).
narrative_ontology:cs_axiom('47a555b5-4e78-40a6-93ea-186ea7a424e0', foundational, sanctioned_domain_development_is_legitimate_latin).
narrative_ontology:cs_axiom_status(sanctioned_domain_development_is_legitimate_latin, holdable).
narrative_ontology:cs_axiom_grounding('47a555b5-4e78-40a6-93ea-186ea7a424e0', sanctioned_domain_development_is_legitimate_latin, instrumental).
narrative_ontology:cs_reference_frame('47a555b5-4e78-40a6-93ea-186ea7a424e0', classical_canon_plus_sanctioned_registers).
narrative_ontology:cs_drift_state('47a555b5-4e78-40a6-93ea-186ea7a424e0', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47a555b5-4e78-40a6-93ea-186ea7a424e0', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__hybrid_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, roman_curia_officials).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, latin_school_educators).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, scientific_nomenclature_bodies).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, medieval_trained_notaries).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, provincial_humanist_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__hybrid_reading, neolatin_community_members).
narrative_ontology:constraint_victim(classical_latin_standard__hybrid_reading, neolatin_community_members).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, canonical_text_authority).
narrative_ontology:constraint_vindicates(classical_latin_standard__hybrid_reading, regulated_innovation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, ratifies, and style-controls the Church's official Latin: encyclicals, canonical texts, dicastery correspondence. Maintains the office that coins and approves new ecclesiastical vocabulary, deciding which post-Classical forms enter official use. Collects a register that is identical in Warsaw and Manila and owned by no member state. Leaving the Curia ends the role; conducting the office in a plurality of vernaculars would surrender the universality claim the register exists to carry.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, roman_curia_officials, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, roman_curia_officials, beneficiary).

% University faculty, critical editors, and lexicographers whose scarce training arbitrates what counts as correct. They examine, emend, publish editions, and referee Neo-Latin composition; salaries, journal authority, and disciplinary standing flow from the scarcity their standard creates. Their professional self-conception was formed by mastery of the canon — a philologist who stopped treating the Classical norm as binding would have dissolved the ground of their own expertise. Retirement is the only exit that does not feel like apostasy.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, classical_philologists, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, classical_philologists, agenda_setter).

% Ran the colleges, gymnasia, and seminary classrooms where the standard was instilled for four centuries: daily composition exercises, correction of faults, declamation contests judged on Ciceronian fidelity. Institutional enrollment and budgets depended on Latin remaining the curricular spine. When vernaculars displaced Latin, these institutions converted their curricula rather than dissolve — the enforcement apparatus shrank with them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, latin_school_educators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, latin_school_educators, beneficiary).

% Chancery clerks, notaries, and university-trained administrators formed in scholastic and curial practice. After the humanist turn their professional prose was relabeled barbarous by arbiters they had never answered to. Keeping their posts meant expensive retraining in Ciceronian style, or enduring correction and ridicule while writing as they had been taught. They could not stop writing Latin without abandoning the livelihood the training was for.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, medieval_trained_notaries, payer,
    moderate, biographical, constrained, continental).

% Scholars outside the Italian and later Parisian and Leiden centers whose usage was marked provincial by metropolitan taste-makers. Correspondence and publication required conforming to arbiters whose verdicts arrived by letter, academy judgment, and journal referee. The alternative — publishing in a vernacular — preserved local readership at the price of the transnational audience the whole point of Latin had been.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, provincial_humanist_scholars, payer,
    moderate, biographical, constrained, continental).

% Botanical, zoological, anatomical, and chemical nomenclature commissions. They inherited a language-neutral naming medium that lets a Swedish botanist name a Brazilian plant for a Japanese reader, and they coin new terms freely under the licensed-domain clause. Their exit is genuinely open — English has progressively replaced Latin in science — and that open door shapes how gently the regime treats them: their coinages are ratified, not corrected.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, scientific_nomenclature_bodies, beneficiary,
    organized, generational, mobile, global).

% Writers and reformers from Dante through the Enlightenment who argued that learned life should proceed in Italian, French, German, and English. They were never seated in the schools, curia, congregations, or academies that adjudicated correctness; their objection was met with silence and answered by history — vernacular print grew around the regime until their position won by default. They are the voices the standard's unanimity presupposed away.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, vernacular_advocates, excluded,
    powerful, generational, mobile, continental).

% Modern voluntary composers of Latin: curial drafting staff, nomenclature code-writers, seminar participants, and online composition communities. They submit prose for correction, absorb stigma for solecisms and barbarisms, and in exchange enjoy the license to coin technical vocabulary the standard grants. Participation is voluntary, yet for many the community and the language are constitutive of avocation or vocation — leaving would mean relinquishing something they are, not something they do.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, neolatin_community_members, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__hybrid_reading, neolatin_community_members, beneficiary).

% Scholars of Neo-Latin and the history of grammar who reconstruct how the standard operated across five centuries: who corrected whom, which forms were ratified, which stigmatized. They hold no stake in the standard's continuation and can see the whole adjudication structure — its beneficiaries, its corrected, its licensed zones — from outside.
narrative_ontology:constraint_stakeholder(classical_latin_standard__hybrid_reading, philological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(classical_latin_standard__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single supranational written register usable across polities and centuries: a Polish cleric corresponds with a Mexican diocese, a Swedish botanist names a Brazilian plant read in Japan, a sixteenth-century commentary stays readable now — without any party adopting another's vernacular. The standard anchors general prose to a fixed canon while licensing technical and ecclesiastical registers to grow through ratified coinage.
% TRANSFER_FUNCTION: Moves linguistic authority and conformity costs from peripheral and provincial writers toward the metropolitan adjudicating center — Roman curia, classical faculties, elite colleges — and moves lexical innovation upward for ratification, with unsanctioned forms paying in correction, stigma, and professional consequence.
% ABSENT_VOICES: Vernacular advocates and medieval-continuity partisans were structurally unseated: the schools, curia, academies, and journals that adjudicated correctness presumed the hybrid standard's premises and did not hear the position that learned life should proceed in the vernaculars, or that transmitted practice alone settles correctness. Women, largely barred from Latin schooling, bore the regime's opportunity costs with no seat at all.
% DISAPPEARANCE_RATIONALE: Canon law's technical vocabulary, biological nomenclature's stability guarantees, the Vatican's language-neutral drafting, and the Neo-Latin communities all presuppose the adjudicated standard. Overnight removal would force every nomenclature code, curial office, and Neo-Latin institution to re-found its norms from scratch, and historical scholarship would lose the shared reference frame that organizes five centuries of Latinate production.
% FOUNDING_PROBLEM: After the fall of the Western Empire, learned communication fragmented: dozens of diverging vernaculars and regional written Latins threatened mutual unintelligibility across the respublica litterarum and the Church. The hybrid answer, consolidated at the humanist turn: anchor correctness to the Classical textual canon, while licensing the technical and ecclesiastical growth that chanceries, universities, monasteries, and the Curia actually needed to function.
% FOUNDING_PROBLEM_CORROBORATION: Chancery archives, university statutes, and the grammatical treatises of the fourteenth and fifteenth centuries independently document the communicative fragmentation the standard addressed; modern nomenclature commissions recite the continuing need for a language-neutral naming medium in their codes' own preambles. Attestation that the founding problem is now largely solved elsewhere comes from outside the benefiting parties: sociolinguists of language shift and the vernacular publication record. No beneficiary-only genealogy stands unchallenged.
narrative_ontology:disappearance_verdict(classical_latin_standard__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(classical_latin_standard__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(classical_latin_standard__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end) because the regime partially delegitimizes — the barbarism verdict carries real professional consequence — but accommodates: licensed domains may coin freely, and the victim set is confined to unsanctioned drift rather than all post-Classical usage. Suppression (0.40) is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. It reflects soft enforcement — pedagogical discipline, editorial gatekeeping, professional stigma — rather than hard coercion, and it never closed the vernacular exit, which is why accessibility_collapse is low (0.38): the alternative of writing in the vernaculars stayed open throughout and eventually took most of the population. Resistance (0.52) was sustained and real: anti-Ciceronian polemic, medieval masters' pushback against humanist correction, the vernacular movement, and modern defenses of medieval Latin. Theater (0.33) is moderate: the teaching, adjudication, and nomenclature functions are real, but ceremonial composition and purity one-upmanship contributed performative layers, peaking in the nineteenth-century school-verse culture. The measurement series run on one shared time grid (1450, 1550, 1650, 1750, 1850, 1950, 2026) with every tracked metric authored at every point. The trajectories are not cyclical: extraction rises to a mid-seventeenth-century peak, declines as vernacular exit drains the coerced population, then upticks at the interval end — the 2026 rise reflects intensification inside the surviving niches (strict nomenclature codes, curial style control) rather than renewed breadth. The suppression_requirement series is authored because the story specifically traces enforcement-capacity change: the humanist correction campaigns, the college-system ratchet, the long decay, and the niche revival. Coordination type is declared information_standard because the founding problem was communicative interoperability across polities and centuries; the identity-policing layer (litterati versus barbari) rides on that standard but is not its load-bearing function. The identity-lock dynamics matter at two seats: classical_philologists are locked by professional identity — the canon's binding force is the ground of their expertise — and neolatin_community_members by communal identity; at both seats, exit would cost selfhood, not just position, which is why the standard persists among volunteers even where its coordinating necessity has faded.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the payer seats — notaries and provincial scholars with constrained exit and high directionality — the regime at its sixteenth-century peak operated as enforced extraction dressed in grammatical necessity: the same structure that ratified the Curia's coinages stigmatized their trained prose. From the mobile beneficiary seat — the nomenclature bodies — the same arrangement computes as near-pure coordination: they receive a functioning international medium, pay little, and can leave. The identity-locked beneficiary seat (classical_philologists) is the interesting case: locked like a target but subsidized like a beneficiary, so the derivation must weigh the lock against the rent. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the Curia (administers and collects), the philologists (rent from scarcity), the colleges (enrollment and budget), and the nomenclature bodies (free coinage, open exit — nearest the beneficiary end of anyone). Victim declarations drive high directionality: medieval_trained_notaries and provincial_humanist_scholars bore the correction, stigma, and retraining costs with constrained exit, placing them near the full-target end during the enforcement peak. Vernacular advocates sit outside the derivation entirely — excluded rather than coordinated; their exclusion is what the unanimity of the adjudicating tribunals presupposed. The neolatin community members straddle: they pay in correction and collect in coinage license, landing near symmetric with an identity-lock pull toward the target side that their voluntariness offsets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Calling the regime pure coordination ignores the documented victims — the notaries and provincials whose careers paid for metropolitan taste — and the active enforcement the barbarism verdict always required. Calling it pure extraction ignores the accommodation that defines this reading: licensed domains, ratified coinage, a victim set confined to unsanctioned drift. Tangled rope holds both facts. On obsolescence: the founding problem is contested, not dead — the communicative fragmentation it solved has been solved otherwise (vernaculars, English), yet the surviving niches still invoke it, and the R5 mismatch consumer reads status=contested against verdict=world_rearranges, which raises no zombie flag. The regime is not a piton: its remaining functions (nomenclature stability, curial neutrality) are performed, not performed-at, and its administrator seats still profit enough to maintain it. The open risk is the modern tail: if the vernacular_exit_selection omega resolves as survivorship rather than accommodation, the residual regime drifts toward inertial maintenance as its population shrinks to the identity-locked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the classical_latin_standard kernel; how would the computed classification shift under the sibling readings?',
    'Generate classical_latin_standard__continuity_reading and classical_latin_standard__reconstruction_reading as separate stories and compare per-seat outputs; the delta in victim sets and epsilon is the measured quantity.',
    'Under continuity_reading the victim set collapses (all transmitted drift is legitimate) and extraction falls toward the coordination-cost floor; under reconstruction_reading the victim set expands to nearly all post-Classical producers and extraction rises sharply. The hybrid''s moderate profile exists only relative to these neighbors — it is not a property of ''correct Latin'' in the abstract.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested kernel; each sibling instantiates a different constraint with a different victim set.').

omega_variable(
    sanctioned_boundary_indeterminacy,
    'Where does the technical/ecclesiastical carve-out end — which post-Classical developments count as licensed development and which as correctable barbarism?',
    'Corpus study of period grammars, censures, and editorial practice: classify contested forms by whether adjudicators treated them as ratified development or as fault, and map the boundary''s movement across the interval.',
    'A wide boundary shrinks the victim set and lowers effective extraction; a narrow boundary grows the victim set and pushes the arrangement toward pure extraction for whoever falls outside the licensed zones. The boundary, not the canon, fixes who pays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctioned_boundary_indeterminacy, conceptual, 'Indeterminacy of the sanctioned-domain boundary that determines the victim set.').

omega_variable(
    vernacular_exit_selection,
    'Is the regime''s declining extraction trajectory genuine accommodation maturing over time, or a selection effect of everyone with viable exit leaving Latin altogether?',
    'Compare correction frequency and stigma severity across populations matched on time but differing in vernacular exit options — cloistered orders against courtly literati, nomenclature boards against humanities faculties.',
    'If selection explains the decline, the residual regime''s mildness reflects a captive-and-volunteer population rather than reformed enforcement, and the modern tail drifts toward inertial maintenance as the population shrinks to the identity-locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_exit_selection, empirical, 'Accommodation versus survivorship in the extraction decline after 1650.').

omega_variable(
    adjudicator_identity_fusion,
    'Does the adjudicating profession maintain the standard because the coordination function still demands it, or because professional identity has fused with the norm?',
    'Track whether norm-maintenance effort tracks communicative demand (nomenclature volume, curial output, Neo-Latin publication) or persists flat as demand falls; compare stated purposes across retired and active adjudicators.',
    'If identity-driven, the modern regime''s persistence is inertial and its performative share will grow; if demand-driven, the standard remains functional coordination inside its niches and the theater ratio should stay bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudicator_identity_fusion, empirical, 'Identity fusion versus functional demand in the standard''s modern maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__hybrid_reading, 1450, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_hybrid_tr_t1450, classical_latin_standard__hybrid_reading, theater_ratio, 1450, 0.2).
narrative_ontology:measurement(cls_hybrid_tr_t1550, classical_latin_standard__hybrid_reading, theater_ratio, 1550, 0.3).
narrative_ontology:measurement(cls_hybrid_tr_t1650, classical_latin_standard__hybrid_reading, theater_ratio, 1650, 0.28).
narrative_ontology:measurement(cls_hybrid_tr_t1750, classical_latin_standard__hybrid_reading, theater_ratio, 1750, 0.32).
narrative_ontology:measurement(cls_hybrid_tr_t1850, classical_latin_standard__hybrid_reading, theater_ratio, 1850, 0.4).
narrative_ontology:measurement(cls_hybrid_tr_t1950, classical_latin_standard__hybrid_reading, theater_ratio, 1950, 0.35).
narrative_ontology:measurement(cls_hybrid_tr_t2026, classical_latin_standard__hybrid_reading, theater_ratio, 2026, 0.33).

% Extraction over time
narrative_ontology:measurement(cls_hybrid_be_t1450, classical_latin_standard__hybrid_reading, base_extractiveness, 1450, 0.42).
narrative_ontology:measurement(cls_hybrid_be_t1550, classical_latin_standard__hybrid_reading, base_extractiveness, 1550, 0.5).
narrative_ontology:measurement(cls_hybrid_be_t1650, classical_latin_standard__hybrid_reading, base_extractiveness, 1650, 0.52).
narrative_ontology:measurement(cls_hybrid_be_t1750, classical_latin_standard__hybrid_reading, base_extractiveness, 1750, 0.5).
narrative_ontology:measurement(cls_hybrid_be_t1850, classical_latin_standard__hybrid_reading, base_extractiveness, 1850, 0.46).
narrative_ontology:measurement(cls_hybrid_be_t1950, classical_latin_standard__hybrid_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement(cls_hybrid_be_t2026, classical_latin_standard__hybrid_reading, base_extractiveness, 2026, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cls_hybrid_su_t1450, classical_latin_standard__hybrid_reading, suppression_requirement, 1450, 0.55).
narrative_ontology:measurement(cls_hybrid_su_t1550, classical_latin_standard__hybrid_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement(cls_hybrid_su_t1650, classical_latin_standard__hybrid_reading, suppression_requirement, 1650, 0.58).
narrative_ontology:measurement(cls_hybrid_su_t1750, classical_latin_standard__hybrid_reading, suppression_requirement, 1750, 0.5).
narrative_ontology:measurement(cls_hybrid_su_t1850, classical_latin_standard__hybrid_reading, suppression_requirement, 1850, 0.42).
narrative_ontology:measurement(cls_hybrid_su_t1950, classical_latin_standard__hybrid_reading, suppression_requirement, 1950, 0.36).
narrative_ontology:measurement(cls_hybrid_su_t2026, classical_latin_standard__hybrid_reading, suppression_requirement, 2026, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__hybrid_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__continuity_reading).
narrative_ontology:affects_constraint(classical_latin_standard__hybrid_reading, classical_latin_standard__reconstruction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'correct Latin' decomposes into three structurally distinct correctness regimes sharing one kernel. This file instantiates the hybrid reading: moderate extraction, a victim set confined to unsanctioned drift, enforcement that peaked with the college system and survives in niches. The continuity reading legitimizes all transmitted drift (victim set near zero, extraction near the coordination floor); the reconstruction reading rejects all post-Classical drift (maximal victim set, high extraction, continuous philological enforcement). The hybrid borrows the reconstruction reading's philological method as its enforcement instrument while granting the continuity reading partial validity inside the sanctioned domains — which is why the relation edges to the two siblings differ. Family links run through network.affects_constraints; each member's epsilon is assessed over its own standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
