% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic-Script Orthographic Regime — Ottoman-Islamic Continuity Reading
 *   domain: political linguistics/state formation/commitment systems
 *
 * SUMMARY:
 *   From the Tanzimat to the republic's first decade, the Ottoman state
 *   maintained Arabic script as the sole authorized medium of official,
 *   legal, religious, and literary writing for the Turkish language. This
 *   story instantiates the CONTINUITY READING of the orthographic kernel: the
 *   script is assessed as the load-bearing link between the Ottoman present
 *   and the Islamic textual past. Per the ε-referent rule, ε is authored for
 *   the standing arrangement under contest — the Arabic-script regime itself
 *   — assessed by this reading's own lights: the reading genuinely values the
 *   continuity function, and precisely on its own terms the costs are visible
 *   — a script poorly matched to Turkish phonology, a written language far
 *   from speech, and a literacy path long enough to gatekeep an empire. The
 *   claim and the metrics are independent authored facts: the reading claims
 *   tangled_rope (genuine corpus-and-worship coordination carrying an
 *   asymmetric literacy tax), and the metrics describe substantially
 *   extractive, actively enforced operation. CONSTRAINT FAMILY: the
 *   colloquial label 'the Ottoman script question' decomposes per the
 *   ε-invariance principle into three readings of one kernel — this
 *   continuity reading, a modernization reading (Latin script as the enabler
 *   of technical statecraft), and a rupture reading (script change as
 *   deliberate severance of the Ottoman-Islamic past). Each is a separate
 *   file with its own ε, victim set, and classification; they are linked
 *   through network.affects_constraints, not merged here. KEY AGENTS (by
 *   structural relationship): - ulema_scholarly_establishment: Primary
 *   beneficiary (institutional/identity_locked) — authority and livelihood
 *   bound to the script-medium of the tradition -
 *   scribal_bureaucracy_kalemiye: Secondary beneficiary and administrator
 *   (institutional/identity_locked) — collects the scarcity premium on script
 *   mastery, administers the standards - ottoman_imperial_state: Agenda
 *   setter (institutional/constrained) — enforces uniformity, collects
 *   dynastic legitimacy - waqf_educational_endowments: Institutional
 *   beneficiary (institutional/constrained) — enrollment and endowment flows
 *   ride script-centered curricula - turkish_literacy_aspirants: Primary
 *   target (powerless/trapped) — bears the literacy-acquisition cost -
 *   state_modernization_reformers: Secondary target (powerful/constrained) —
 *   bears the blocked-reform cost - millet_minority_communities: Excluded
 *   actor (organized/mobile) — runs parallel alphabets, outside the
 *   conversation - orientalist_foreign_observers: Analytical observer
 *   (analytical/analytical) — documents costs and debates from outside
 *
 * KEY AGENTS:
 *   - ulema_scholarly_establishment: primary beneficiary (institutional/identity_locked) — custodial authority over the script-medium
 *   - scribal_bureaucracy_kalemiye: secondary beneficiary and administrator (institutional/identity_locked) — collects employment rents, drafts the standards
 *   - ottoman_imperial_state: agenda setter (institutional/constrained) — enforces the orthographic requirement, collects legitimacy
 *   - waqf_educational_endowments: institutional beneficiary (institutional/constrained) — curricula and endowment flows tied to script teaching
 *   - turkish_literacy_aspirants: primary target (powerless/trapped) — bears the multi-year literacy tax
 *   - state_modernization_reformers: secondary target (powerful/constrained) — bears the blocked-reform cost
 *   - millet_minority_communities: excluded actor (organized/mobile) — living proof of functioning alternatives, never consulted
 *   - orientalist_foreign_observers: analytical observer (analytical/analytical) — external documentation of costs and debates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.7).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic-Script Orthographic Regime — Ottoman-Islamic Continuity Reading").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political linguistics/state formation/commitment systems").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '69d17c9f-05f9-4607-86fa-467e1ea0e16f').
narrative_ontology:cs_kernel_codification('69d17c9f-05f9-4607-86fa-467e1ea0e16f', fixed_text).
narrative_ontology:cs_authority_grounding('69d17c9f-05f9-4607-86fa-467e1ea0e16f', lineage).
narrative_ontology:cs_interpretation_layer_present('69d17c9f-05f9-4607-86fa-467e1ea0e16f').
narrative_ontology:cs_reading_relation('69d17c9f-05f9-4607-86fa-467e1ea0e16f', orthographic_kernel__modernization_reading, influences).
narrative_ontology:cs_reading_relation('69d17c9f-05f9-4607-86fa-467e1ea0e16f', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('69d17c9f-05f9-4607-86fa-467e1ea0e16f', foundational, script_continuity_constitutes_identity).
narrative_ontology:cs_axiom_status(script_continuity_constitutes_identity, holdable).
narrative_ontology:cs_axiom_grounding('69d17c9f-05f9-4607-86fa-467e1ea0e16f', script_continuity_constitutes_identity, deontological).
narrative_ontology:cs_axiom('69d17c9f-05f9-4607-86fa-467e1ea0e16f', foundational, quranic_script_medium_inviolable).
narrative_ontology:cs_axiom_status(quranic_script_medium_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('69d17c9f-05f9-4607-86fa-467e1ea0e16f', quranic_script_medium_inviolable, theological).
narrative_ontology:cs_reference_frame('69d17c9f-05f9-4607-86fa-467e1ea0e16f', unbroken_islamic_textual_lineage).
narrative_ontology:cs_drift_state('69d17c9f-05f9-4607-86fa-467e1ea0e16f', tanzimat_to_republic_reform_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('69d17c9f-05f9-4607-86fa-467e1ea0e16f', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ulema_scholarly_establishment).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, scribal_bureaucracy_kalemiye).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, ottoman_imperial_state).
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, waqf_educational_endowments).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, turkish_literacy_aspirants).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, state_modernization_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the Qur'an, jurisprudence, and the adab corpus, all transmitted in Arabic script. Their judicial posts, teaching chairs, and social standing require mastery of the script and its commentarial apparatus. Leaving the tradition would mean abandoning vocation, community standing, and the interpretive authority they have spent decades acquiring.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ulema_scholarly_establishment, beneficiary,
    institutional, generational, identity_locked, continental).

% The chancery corps trained in siyakat and divani hands. Employment in the financial and clerical services is gated by script proficiency that takes years to acquire, so scarcity of that proficiency sets the price of entry into their profession. They also draft and administer the documentary standards that define correct official writing. Their accumulated human capital has almost no value outside the script regime.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, scribal_bureaucracy_kalemiye, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, scribal_bureaucracy_kalemiye, agenda_setter).

% Sets and enforces the orthographic requirement: official documents, courts, and schools operate in Arabic script, and the state certifies teachers and validates documents accordingly. Dynastic legitimacy is anchored in guardianship of the Islamic textual tradition, so abandoning the script would undercut the state's own claim to rule. Switching media is available in principle but carries a legitimacy price the dynasty is unwilling to pay while the continuity frame holds.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_imperial_state, agenda_setter,
    institutional, generational, constrained, continental).

% Pious foundations finance mekteps and medreses whose curricula center on script acquisition and recitation. Enrollment, stipends, and endowment income all flow through institutions whose product is Arabic-script literacy; a changed orthography would strand their buildings, staff, and charters.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, waqf_educational_endowments, beneficiary,
    institutional, generational, constrained, continental).

% Villagers and townspeople who want to read. The script maps poorly onto Turkish vowels and the written language carries heavy Arabic and Persian vocabulary, so functional literacy demands years of mechanical drill before meaningful reading begins. Alternative schooling barely exists for Muslim commoners, and the spoken-written distance means even oral fluency does not shorten the path. Most remain outside literacy altogether; those who enter pay with the better part of a childhood.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, turkish_literacy_aspirants, payer,
    powerless, biographical, trapped, national).

% Tanzimat and post-Tanzimat officials, military officers, engineers, and journalists who need telegraph codes, printing economies, standardized accounting, and translated science. Every proposal to adapt or replace the script — from mid-century memoranda to Enver Pasha's 1913 standardization — is defeated in the councils where the ulema and chancery hold standing. Their careers are bound to the state they are trying to modernize, so exit means surrendering the lever they exist to pull.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernization_reformers, payer,
    powerful, generational, constrained, continental).

% Greek, Armenian, and other communities run parallel school systems in their own alphabets under the millet arrangement. They demonstrate daily that alternative orthographies function inside the same economy, yet they are not consulted in imperial orthography debates and their script independence is treated as a fact to tolerate rather than a precedent to generalize.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, millet_minority_communities, excluded,
    organized, generational, mobile, regional).

% European scholars, consular translators, and technical advisors who document literacy rates, printing costs, and the recurring reform debates. They correspond with reformers, publish comparisons with Latin-alphabet societies, and hold no position inside the arrangement whose costs they measure.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, orientalist_foreign_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, scribal_bureaucracy_kalemiye).
narrative_ontology:fixing_cost_class(orthographic_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single written medium carries administration, law, worship, and literature across a multilingual, multi-confessional empire, and ties Turkish literate production to the pan-Islamic Perso-Arabic corpus — one orthography instead of many, and an inherited library of fourteen centuries accessible to anyone trained in it.
% TRANSFER_FUNCTION: Moves years of training time and literacy-access costs from learners and reform budgets to the established literate elite, which collects a scarcity premium on script mastery (posts, fees, interpretive authority), and to script-teaching institutions, which collect enrollment and endowment flows.
% ABSENT_VOICES: The illiterate majority whose exclusion the arrangement maintains has no seat anywhere in the chancery-madrasa conversation; women are largely barred from the advanced literacy the arrangement gates; minority-script communities speak from parallel systems but are never asked whether their precedent should generalize. Their objections surface only obliquely, through the reformist press.
% DISAPPEARANCE_RATIONALE: If the Arabic-script requirement vanished overnight, chancery procedure, judicial record, schooling, and religious publication would all reorganize around whatever medium replaced it; the scribal corps' scarcity premium would evaporate, waqf curricula would strand, and the corpus link would survive only for specialists — the entire literate order rearranges, which is precisely what happened after 1928.
% FOUNDING_PROBLEM: Bind a Turkic-speaking polity into Islamic textual civilization — giving it the medium of revelation, law, and the classical sciences — and administer a vast empire through one chancery medium legible from Bosnia to Basra.
% FOUNDING_PROBLEM_CORROBORATION: The ulema attest the founding problem is live, but they are its beneficiaries and their attestation is discounted accordingly. Corroboration from outside the benefiting set: the reformist Ottoman press (Gaspıralı's Tercüman, Young Ottoman and Unionist writings) attests the civilizational-linkage problem was real while arguing its costs had come to outweigh it; foreign technical advisors and orientalist observers independently document the literacy and printing costs. No seat is fully neutral; the nearest-to-neutral attestations agree the linkage was genuine and dispute only whether it still justified the price.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.70 at interval end) because the arrangement's costs are decoupled from its coordination yield for most participants: the phonology mismatch and the spoken-written gulf make literacy acquisition a multi-year tax, while the returns concentrate in a small trained elite. Suppression (0.62) is authored as a raw structural property — the educational monopoly, the official-document requirement, and religious sanction against altering the medium of revelation — and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by the engine, through directionality and scope. Theater ratio (0.30) reflects a functional core that is real — administration, worship, and corpus access all genuinely run through the script — with a growing defensive-symbolic share: calligraphic revival and sanctity arguments deployed against reform rather than for communication. Accessibility collapse is moderate (0.45): millet alphabets, oral transmission, and intra-script simplification proposals kept alternatives partly alive; the regime never collapsed them completely. Resistance (0.58) is substantial and recurring — the reform debate runs continuously from the Tanzimat memoranda through Gaspıralı's columns to Enver Pasha's 1913 standardization attempt. The temporal series run on one shared eight-point grid (1839–1928) so every tracked metric is authored at every examined time point; all three trajectories rise together as modernization pressure grew faster than the arrangement could absorb it, with enforcement hardening rather than relaxing under challenge.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the ulema seat the arrangement is a sacred trust with near-zero personal cost — a beneficiary with identity-locked exit experiences subsidy, not burden. From the chancery seat it is a profession: the same difficulty that taxes learners sets the scarcity price of scribal employment. From the palace seat it is legitimacy infrastructure. From the aspirant seat — powerless, trapped — the identical structure is a wall that keeps most of the population outside literacy entirely. From the reformer seat — powerful but constrained — it is a blocked corridor: the agents with the most state capacity are precisely the ones forbidden to use it on the script. Same constraint, four incompatible lived types; the engine derives this divergence from the structural data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: ulema, chancery, state, and waqfs sit near the beneficiary end (low d), with identity-locked exits pinning the ulema and scribes especially deep into subsidy territory — their human capital and selfhood have no value outside the regime. Victim declarations drive the opposite pole: literacy aspirants (trapped, powerless) sit near the full-target end, and modernization reformers, despite institutional-grade power, are held near it by constrained exit — power does not buy them an exit from the state whose instrument the script is. Continental scope modestly amplifies effective extraction by raising verification difficulty across the empire's breadth. Millet communities derive near-symmetric: they neither collect from nor pay into the Arabic-script regime directly, running parallel systems at the margin.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding a Turkic-speaking polity into Islamic textual civilization and administering an empire through one chancery medium — was genuinely solved once and remained partially live throughout the interval: worship, law, and the corpus genuinely required the medium, which is why even committed reformers proposed compromises inside Arabic script before proposing escape from it. But its status became contested as a second problem — technical state survival — grew alongside it, and the two problems came to demand incompatible media. The arrangement did not atrophy into performance; it was terminated by rupture in 1928 while still functionally maintained, so no mandatrophy_resolved declaration is authored. The tangled_rope classification prevents mislabeling in both directions: a pure-rope verdict would erase the literacy tax and the blocked reform path that the beneficiary seats' own defense of the arrangement made visible; a pure-snare verdict would erase the corpus-and-worship coordination that even the arrangement's opponents acknowledged when they built their compromises inside it. The hybrid is the honest center, and the receipt surface locates the capture: the chancery-ulema literate complex demonstrably accrues the gains, and fixing was prohibitively priced for the one seat that could fix it — the state — for exactly as long as the continuity frame held. When the frame broke in 1928, the same fix executed in months, which is the cleanest possible demonstration that the prohibition was frame-dependent rather than technical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (continuity_reading) of the orthographic_kernel. Would instantiating the modernization_reading or the rupture_reading instead change the victim set, the epsilon, and the computed type?',
    'Author and cross-read the sibling stories: the modernization_reading (epsilon referent: the script regime as obstacle; victims: modernization projects and their publics) and the rupture_reading (epsilon referent: the script regime as cultural apparatus; victims: the new national subject to be created). Compare the three epsilon/victim structures and the per-seat classifications each produces.',
    'Under the modernization reading the arrangement computes as harder-edged extraction with the suppressed reform path as the named harm; under the rupture reading intent enters the structure and the beneficiary set shifts toward the state-as-severer. The tangled_rope verdict authored here holds only under the continuity reading''s own lights; the engine''s cross-story comparison is what adjudicates the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the orthographic kernel governs the classification; sibling readings are separate constraints, not parameters of this one.').

omega_variable(
    script_difficulty_origin,
    'Is the script''s costliness for Turkish intrinsic to the phonology mismatch, or a contingent product of pedagogy and the refusal to adapt the script short of abandonment (systematic vowel marking, letter reform, simplified pedagogical editions)?',
    'Comparative history of intra-script adaptations — Enver Pasha''s 1913 standardization, vowel-marked teaching editions, the Karamanlidika parallel in Greek-letter Turkish — and their measured effects on acquisition time and literacy rates.',
    'If the difficulty is contingent, the extraction component is constructed rent and the balance tilts toward snare; if intrinsic, part of epsilon is irreducible coordination cost and the rope component is larger than the scalar suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_difficulty_origin, empirical, 'Whether the literacy tax is built into the script or built by its maintainers.').

omega_variable(
    counterfactual_voluntary_persistence,
    'Would the Arabic-script regime have persisted among writers absent state enforcement, or did it require continuous coercive maintenance?',
    'Natural experiments: private publishing and correspondence practices in periods of lax enforcement; minority-script adoption rates; and the speed and completeness of mass conversion after 1928 once enforcement reversed direction.',
    'Voluntary persistence supports a rope-weighted reading (network-effect coordination around an installed corpus); enforcement-dependent persistence supports the extraction-weighted reading and raises the effective suppression attributable to the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_voluntary_persistence, empirical, 'Whether the arrangement was self-sustaining coordination or coercion-held extraction.').

omega_variable(
    sanctity_vs_gatekeeping,
    'How much of the ulema-led defense of the script reflects doctrinal commitment to the medium of revelation, and how much reflects material interest in gatekeeping literacy?',
    'Discriminant analysis of ulema positions across reform types: reforms that preserved sanctity while lowering barriers (vowel-marked Qur''an pedagogy, printed simplified editions) versus reforms that threatened scribal livelihoods directly. Divergent treatment of the two classes separates doctrine from rent-defense.',
    'Predominantly doctrinal defense shifts the suppression picture toward the ideological/internalized and softens the capture reading; predominantly rent-defense confirms the beneficiary-driven enforcement structure and hardens the tangled_rope-toward-snare gradient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_vs_gatekeeping, empirical, 'Whether the enforcement coalition is moved by theology, by interest, or by a fusion the two cannot be separated from.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 1839, 1928).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t1839, orthographic_kernel__continuity_reading, theater_ratio, 1839, 0.12).
narrative_ontology:measurement(orth_tr_t1856, orthographic_kernel__continuity_reading, theater_ratio, 1856, 0.14).
narrative_ontology:measurement(orth_tr_t1867, orthographic_kernel__continuity_reading, theater_ratio, 1867, 0.16).
narrative_ontology:measurement(orth_tr_t1876, orthographic_kernel__continuity_reading, theater_ratio, 1876, 0.18).
narrative_ontology:measurement(orth_tr_t1897, orthographic_kernel__continuity_reading, theater_ratio, 1897, 0.21).
narrative_ontology:measurement(orth_tr_t1908, orthographic_kernel__continuity_reading, theater_ratio, 1908, 0.25).
narrative_ontology:measurement(orth_tr_t1913, orthographic_kernel__continuity_reading, theater_ratio, 1913, 0.28).
narrative_ontology:measurement(orth_tr_t1928, orthographic_kernel__continuity_reading, theater_ratio, 1928, 0.3).

% Extraction over time
narrative_ontology:measurement(orth_be_t1839, orthographic_kernel__continuity_reading, base_extractiveness, 1839, 0.52).
narrative_ontology:measurement(orth_be_t1856, orthographic_kernel__continuity_reading, base_extractiveness, 1856, 0.55).
narrative_ontology:measurement(orth_be_t1867, orthographic_kernel__continuity_reading, base_extractiveness, 1867, 0.58).
narrative_ontology:measurement(orth_be_t1876, orthographic_kernel__continuity_reading, base_extractiveness, 1876, 0.61).
narrative_ontology:measurement(orth_be_t1897, orthographic_kernel__continuity_reading, base_extractiveness, 1897, 0.64).
narrative_ontology:measurement(orth_be_t1908, orthographic_kernel__continuity_reading, base_extractiveness, 1908, 0.67).
narrative_ontology:measurement(orth_be_t1913, orthographic_kernel__continuity_reading, base_extractiveness, 1913, 0.69).
narrative_ontology:measurement(orth_be_t1928, orthographic_kernel__continuity_reading, base_extractiveness, 1928, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t1839, orthographic_kernel__continuity_reading, suppression_requirement, 1839, 0.44).
narrative_ontology:measurement(orth_su_t1856, orthographic_kernel__continuity_reading, suppression_requirement, 1856, 0.47).
narrative_ontology:measurement(orth_su_t1867, orthographic_kernel__continuity_reading, suppression_requirement, 1867, 0.5).
narrative_ontology:measurement(orth_su_t1876, orthographic_kernel__continuity_reading, suppression_requirement, 1876, 0.53).
narrative_ontology:measurement(orth_su_t1897, orthographic_kernel__continuity_reading, suppression_requirement, 1897, 0.56).
narrative_ontology:measurement(orth_su_t1908, orthographic_kernel__continuity_reading, suppression_requirement, 1908, 0.59).
narrative_ontology:measurement(orth_su_t1913, orthographic_kernel__continuity_reading, suppression_requirement, 1913, 0.61).
narrative_ontology:measurement(orth_su_t1928, orthographic_kernel__continuity_reading, suppression_requirement, 1928, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Ottoman script question' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-member constraint family sharing the orthographic_kernel. This file is the continuity_reading (upstream/incumbent: highest empirical confidence in the corpus-linkage fact, contested normative valence). The modernization_reading accepts the continuity fact but denies its priority against statecraft needs; the rupture_reading affirms the continuity fact and inverts its valence into a reason for severance. Edges: this reading INFLUENCES the modernization reading — decades of continuity defense raised the political cost of Latin proposals and channeled reformers into intra-script compromises (culminating in Enver Pasha's 1913 standardization), shaping the sibling's operating environment without resolving the dispute. The upstream claim (the script genuinely binds the textual tradition) is cited as evidence by both siblings, which is why the family is linked rather than independent. Epsilon differs across members because each reading assesses a different instantiation of the arrangement under contest; no member hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
