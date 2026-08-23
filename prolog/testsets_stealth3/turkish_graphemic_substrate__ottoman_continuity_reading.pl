% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__ottoman_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__ottoman_continuity_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__ottoman_continuity_reading
 *   human_readable: Arabic-Script Legitimacy Mandate (Ottoman Continuity Reading)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   This file instantiates ONE reading - the ottoman_continuity_reading - of
 *   the contested kernel turkish_graphemic_substrate. The standing
 *   arrangement under contest is the pre-1928 Ottoman order in which Arabic
 *   script is the sole legitimate graphemic substrate for written Turkish,
 *   administered through the religious-educational establishment, print
 *   regulation, and scribal guilds. Epsilon's referent is that standing
 *   arrangement, assessed by this reading's own lights: the reading endorses
 *   the substrate as civilizational continuity, so it authors moderate
 *   extraction (0.45) - acknowledging the acquisition bottleneck and the
 *   schooling returns flowing to the establishment while holding the script
 *   itself legitimate. The sibling readings are separate constraints in
 *   separate files: the secular_nationalist_reading assesses the same
 *   pre-1928 arrangement from a hostile seat and authors substantially higher
 *   epsilon (clerical rent, civilizational rupture); the
 *   gradual_transition_reading authors intermediate epsilon over a
 *   dual-script counterfactual. Same referent, reading-indexed values; the
 *   family is linked via network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - ottoman_ulema: agenda-setting beneficiary
 *   (institutional/identity_locked) - administers the literacy channel and
 *   collects its returns - calligraphers_and_manuscript_guilds: beneficiary
 *   (organized/identity_locked) - embodied craft fused to the script -
 *   arabic_script_printers: beneficiary (organized/constrained) - sunk
 *   capital in type and backlists - ottoman_literati: beneficiary
 *   (organized/constrained) - careers and archives inseparable from the
 *   register - commercial_classes: primary target (powerful/constrained) -
 *   bears the cross-boundary transaction cost - secular_intelligentsia:
 *   target (organized/constrained) - bears the modernization blockage -
 *   rural_and_female_population: diffuse target (powerless/trapped) - bears
 *   the illiteracy burden itself - reform_faction_officials: excluded
 *   challenger (powerful/mobile) - outside the conversation until it seizes
 *   the state - orientalist_scholars: analytical observer - documents
 *   structure from outside all factions
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__ottoman_continuity_reading, 0.45).
domain_priors:suppression_score(turkish_graphemic_substrate__ottoman_continuity_reading, 0.62).
domain_priors:theater_ratio(turkish_graphemic_substrate__ottoman_continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__ottoman_continuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__ottoman_continuity_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__ottoman_continuity_reading, "Arabic-Script Legitimacy Mandate (Ottoman Continuity Reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__ottoman_continuity_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__ottoman_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__ottoman_continuity_reading, 'df14c403-345b-497c-b5a3-ed301826bff3').
narrative_ontology:cs_kernel_codification('df14c403-345b-497c-b5a3-ed301826bff3', fixed_text).
narrative_ontology:cs_authority_grounding('df14c403-345b-497c-b5a3-ed301826bff3', lineage).
narrative_ontology:cs_interpretation_layer_present('df14c403-345b-497c-b5a3-ed301826bff3').
narrative_ontology:cs_reading_relation('df14c403-345b-497c-b5a3-ed301826bff3', turkish_graphemic_substrate__secular_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('df14c403-345b-497c-b5a3-ed301826bff3', turkish_graphemic_substrate__gradual_transition_reading, coexists_with).
narrative_ontology:cs_axiom('df14c403-345b-497c-b5a3-ed301826bff3', foundational, civilizational_identity_requires_sacral_script).
narrative_ontology:cs_axiom_status(civilizational_identity_requires_sacral_script, holdable).
narrative_ontology:cs_axiom_grounding('df14c403-345b-497c-b5a3-ed301826bff3', civilizational_identity_requires_sacral_script, theological).
narrative_ontology:cs_axiom('df14c403-345b-497c-b5a3-ed301826bff3', secondary, corpus_accessibility_overrides_acquisition_cost).
narrative_ontology:cs_axiom_status(corpus_accessibility_overrides_acquisition_cost, holdable).
narrative_ontology:cs_axiom_grounding('df14c403-345b-497c-b5a3-ed301826bff3', corpus_accessibility_overrides_acquisition_cost, instrumental).
narrative_ontology:cs_reference_frame('df14c403-345b-497c-b5a3-ed301826bff3', ottoman_islamic_scribal_continuum).
narrative_ontology:cs_drift_state('df14c403-345b-497c-b5a3-ed301826bff3', late_reformist_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('df14c403-345b-497c-b5a3-ed301826bff3', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_ulema).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, calligraphers_and_manuscript_guilds).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, arabic_script_printers).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literati).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, commercial_classes).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, secular_intelligentsia).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__ottoman_continuity_reading, rural_and_female_population).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_islamic_civilizational_continuity_doctrine).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__ottoman_continuity_reading, sacral_script_transmission_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff and administer the mosque-primary schools and medreses through which nearly all written Turkish is acquired, certify teachers, and adjudicate correct usage in law and preaching. Schooling fees, endowed posts, and social standing flow to the establishment through its custody of the written word; abandoning that custody would dissolve the institutions, endowments, and transmission lineages that constitute the position.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_ulema, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_ulema, beneficiary).

% Produce manuscripts, official documents, and mosque inscriptions; skill passes through master-apprentice lineages requiring a decade or more per grade. The accumulated craft exists only in this script; work in another alphabet would erase a lifetime's embodied training and the guild's market.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, calligraphers_and_manuscript_guilds, beneficiary,
    organized, biographical, identity_locked, regional).

% Run presses whose type stock, compositor training, and backlists all assume the script; conversion would strand capital and idle skilled crews.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, arabic_script_printers, beneficiary,
    organized, biographical, constrained, regional).

% Divan poets, court historians, and chancery officials whose reputations, patronage, and archives exist entirely in the classical written register; their readership and their life's work are inseparable from the script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_literati, beneficiary,
    organized, biographical, constrained, national).

% Merchants and financiers trading with Europe keep parallel French correspondence, employ translator-scribes, and absorb a cost on every document crossing the script boundary. Petitions to the Porte for a practical alphabet went unanswered for decades; the domestic written environment offers them no exit.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, commercial_classes, payer,
    powerful, biographical, constrained, continental).

% Journalists, teachers, and staff officers arguing that administration and science need an alphabet matched to spoken Turkish; they publish critiques under censorship risk and hold no formal seat in any body that decides script policy.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, secular_intelligentsia, payer,
    organized, generational, constrained, national).

% The large majority of the population, unschooled or minimally schooled; the available literacy channel runs through religious institutions that many villages lack and many households will not use for daughters. They carry the heaviest illiteracy burden and have never been asked about the script.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, rural_and_female_population, payer,
    powerless, biographical, trapped, national).

% Unionist and Republican cadres kept outside the script-legitimacy conversation for most of the interval; they publish reform schemes, build parallel administrative and military positions, and in 1926-1928 seize the state and abolish the arrangement by decree.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, reform_faction_officials, excluded,
    powerful, generational, mobile, national).

% European academics cataloguing Ottoman manuscripts and surveying literacy; they correspond with reformers, publish comparisons of acquisition times across scripts, and document both the corpus's depth and the bottleneck - outside any domestic faction.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__ottoman_continuity_reading, orientalist_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__ottoman_continuity_reading, ottoman_ulema).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__ottoman_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single sacral script coordinates an empire-spanning Islamic textual sphere - law, religion, poetry, chancery administration - from the Balkans to Arabia, preserves reader access to a millennium-long corpus, and links Turkish Muslims to the written tradition of the wider Islamic world.
% TRANSFER_FUNCTION: Moves literacy access and cultural authority: custody of the acquisition path and its returns flows to the religious-educational establishment; the costs of acquisition and of every European-facing adaptation flow to merchants, modernizing professionals, and the unschooled population.
% ABSENT_VOICES: The unschooled majority, especially rural women, never sat in any script-legitimacy conversation; commercial interests and reformist educators were heard only at the margins through petitions and censored press until the 1920s. Nearly every authoritative voice inside the conversation belonged to the arrangement's own beneficiary coalition.
% DISAPPEARANCE_RATIONALE: If the Arabic-script mandate vanished overnight, the religious-educational complex would lose its monopoly over literacy, print markets would convert within years, the Ottoman manuscript corpus would become inaccessible to new readers within two generations, and ties to the wider Islamic textual world would thin - approximately the rearrangement the 1928 abolition actually produced.
% FOUNDING_PROBLEM: Governing a multi-confessional, multilingual empire with one prestigious script tied to Islamic law and dynastic legitimacy: guaranteeing Quranic literacy, staffing a chancery, and coordinating administration across linguistic diversity through a single elite written medium.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European orientalist literacy surveys and missionary school reports document both the corpus's depth and the acquisition bottleneck; chamber-of-commerce petitions from Istanbul, Salonica, and Izmir attest the commercial cost; the reformist press (Ictihad, Tanin) attests the modernization argument. The ulema attest the founding problem as live from inside the beneficiary set; the external sources dispute whether the imperial-coordination problem survived the empire itself.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__ottoman_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__ottoman_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__ottoman_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__ottoman_continuity_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).
:- end_tests(turkish_graphemic_substrate__ottoman_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.45 at interval end) because even from this endorsing seat the arrangement imposes a real acquisition burden and channels schooling returns to the establishment; the reading's own honesty about the bottleneck keeps epsilon well above zero. Suppression (0.62) reflects the structural schooling monopoly and censorship of reform proposals - suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled by the engine. Theater (0.30) rises modestly as ceremonial script-prestige maintenance grows while functional reach lags modernizing demand. Accessibility collapse is moderate (0.45): alternatives (French schooling, minority-community scripts, private tutoring) persist but sit outside the legitimate order. Resistance (0.60) is substantial: merchant petitions, the reformist press, Unionist script experiments. All three tracked metrics share one time grid {0,7,14,21,28}; the drift is monotonic, driven by the widening gap between the script order and commercial-scientific demand, not cyclical. Enforcement intensifies through the interval (suppression_requirement 0.44 to 0.62) as the establishment defends the order against reform agitation, and the series ends at the 1928 abolition point when enforcement capacity collapsed with the regime change. Coalition note: the largest paying seat (rural and female population) had no coalition capacity; relief arrived top-down through regime seizure, not bottom-up aggregation.
 *
 * PERSPECTIVAL GAP:
 *   From the ulema seat the arrangement is sacred transmission it administers; from the commercial and intelligentsia seats it is a toll on every transaction crossing the script boundary; from the unschooled majority's seat it is simply a wall. One structure, computed differently per seat from power, exit, and declared position - the authored claim does not adjudicate the divergence; the engine measures it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly onto the derivation chain: the four beneficiary seats derive low d (the ulema lowest - agenda-setter collecting directly, identity-locked to the transmission role); the three paying seats derive high d, modulated by exit - the trapped rural and female population sits nearest the full-target end, powerful merchants are damped slightly by partial French arbitrage but remain target-side because their domestic written environment offers no exit; the excluded reform faction and the analytical observer feed no directionality. No directionality overrides were needed: beneficiary/victim declarations plus exit options already yield the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - coordinating an empire-spanning Islamic textual sphere through one sacral script - was still live when the arrangement was terminated; the constraint did not atrophy into performance and linger. It was abolished by a hostile regime while still functioning. Hence no mandatrophy_resolved declaration: the mismatch consumer reads founding_problem_status=contested crossed with disappearance_verdict=world_rearranges, which flags a contested termination, not a zombie. Classification discipline: a rope claim would hide the bottleneck rents behind the genuine coordination function; a snare claim would deny the real pan-Islamic coordination the script performed for centuries. The tangled_rope claim holds both faces, and the metrics carry the asymmetry independently of the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the turkish_graphemic_substrate kernel (reading: ottoman_continuity_reading). What structural delta would adoption of a sibling reading produce?',
    'Political resolution of the script contest - compare the pre- and post-1928 arrangements: the secular_nationalist_reading inverts the directionality table (ulema and calligraphic guilds become the paying seats of the Latin mandate; commercial and modernizing seats are relieved); the gradual_transition_reading spreads extraction across a dual-script period. Each sibling is a separate file with its own epsilon, beneficiaries, and victims.',
    'Sibling adoption re-authors epsilon and swaps the beneficiary/victim sets; classifications computed from this file do not transfer to the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of three readings of the graphemic-substrate kernel; the contest''s resolution relocates extraction across the seat table.').

omega_variable(
    continuity_vs_constructed_interest,
    'Is Arabic-script legitimacy a genuine civilizational continuity claim, or a constructed doctrine serving identifiable institutional interests?',
    'Trace whether enforcement intensity across the interval tracks doctrinal commitment or institutional revenue and employment dependence - e.g., whether the establishment''s defense hardened fastest where endowed-post income was concentrated.',
    'If constructed interest dominates, the arrangement shifts toward pure extraction despite this reading''s endorsement; if doctrinal commitment dominates, the coordination component is genuine and durable and the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_constructed_interest, empirical, 'Naturalness ambiguity: civilizational continuity versus institutional self-interest in the legitimacy doctrine.').

omega_variable(
    literacy_bottleneck_attribution,
    'How much of the era''s low literacy is attributable to the script''s acquisition cost versus schooling provision, poverty, and gender exclusion?',
    'Comparative acquisition-time studies across script systems; post-1928 cohort literacy holding schooling provision constant; literacy levels in minority communities using other scripts under comparable poverty.',
    'If the script is a minor factor, measured extraction falls toward pure coordination; if it is a major factor, the bottleneck-rent component rises and the arrangement approaches pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_bottleneck_attribution, empirical, 'Attribution of the literacy bottleneck between script difficulty and provision failure.').

omega_variable(
    ulema_identity_lock_durability,
    'Is the establishment''s defense of the script constitutive of institutional identity such that no compensation could buy adaptation, or a priced position open to settlement?',
    'Counterfactual evidence from the reform aftermath: observe whether defeated religious authorities adapted to the Latin mandate once coercion settled (some produced Latin-script religious calendars and commentary) or persisted in unofficial Arabic-script instruction.',
    'If identity-locked, the arrangement''s fall required regime change rather than bargaining; if priced, a gradual-transition settlement was always available and its rejection indicts the agenda-setters'' preferences rather than structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ulema_identity_lock_durability, conceptual, 'Durability of the religious establishment''s identity-lock to the script.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__ottoman_continuity_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ottoman_continuity_reading_tr_t0, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(ottoman_continuity_reading_tr_t0, observed).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t7, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement_basis(ottoman_continuity_reading_tr_t7, observed).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t14, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement_basis(ottoman_continuity_reading_tr_t14, observed).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t21, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 21, 0.27).
narrative_ontology:measurement_basis(ottoman_continuity_reading_tr_t21, observed).
narrative_ontology:measurement(ottoman_continuity_reading_tr_t28, turkish_graphemic_substrate__ottoman_continuity_reading, theater_ratio, 28, 0.3).
narrative_ontology:measurement_basis(ottoman_continuity_reading_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(ottoman_continuity_reading_be_t0, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(ottoman_continuity_reading_be_t0, observed).
narrative_ontology:measurement(ottoman_continuity_reading_be_t7, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 7, 0.36).
narrative_ontology:measurement_basis(ottoman_continuity_reading_be_t7, observed).
narrative_ontology:measurement(ottoman_continuity_reading_be_t14, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 14, 0.39).
narrative_ontology:measurement_basis(ottoman_continuity_reading_be_t14, observed).
narrative_ontology:measurement(ottoman_continuity_reading_be_t21, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 21, 0.42).
narrative_ontology:measurement_basis(ottoman_continuity_reading_be_t21, observed).
narrative_ontology:measurement(ottoman_continuity_reading_be_t28, turkish_graphemic_substrate__ottoman_continuity_reading, base_extractiveness, 28, 0.45).
narrative_ontology:measurement_basis(ottoman_continuity_reading_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(ottoman_continuity_reading_su_t0, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(ottoman_continuity_reading_su_t0, observed).
narrative_ontology:measurement(ottoman_continuity_reading_su_t7, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 7, 0.49).
narrative_ontology:measurement_basis(ottoman_continuity_reading_su_t7, observed).
narrative_ontology:measurement(ottoman_continuity_reading_su_t14, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 14, 0.53).
narrative_ontology:measurement_basis(ottoman_continuity_reading_su_t14, observed).
narrative_ontology:measurement(ottoman_continuity_reading_su_t21, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement_basis(ottoman_continuity_reading_su_t21, observed).
narrative_ontology:measurement(ottoman_continuity_reading_su_t28, turkish_graphemic_substrate__ottoman_continuity_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement_basis(ottoman_continuity_reading_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__ottoman_continuity_reading, information_standard).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__secular_nationalist_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__ottoman_continuity_reading, turkish_graphemic_substrate__gradual_transition_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Turkish script question' covers three structurally distinct claims. This file (ottoman_continuity_reading) authors epsilon ~0.45 for the standing Arabic-script arrangement from an endorsing seat; the secular_nationalist_reading authors substantially higher epsilon for the same referent from a hostile seat; the gradual_transition_reading authors intermediate epsilon over a dual-script counterfactual. Same referent, reading-indexed values - one story per reading, linked by network edges. The upstream reading (this one) supplies the legitimacy doctrine the downstream readings define themselves against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
