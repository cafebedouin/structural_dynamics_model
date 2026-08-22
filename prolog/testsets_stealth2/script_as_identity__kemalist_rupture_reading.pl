% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Kemalist Rupture Reading — Latin Script Mandate as Civilizational Severance
 *   domain: political/state-building/comparative-linguistics
 *
 * SUMMARY:
 *   On 1 November 1928 the Grand National Assembly enacted Law 1353,
 *   replacing the Arabic script with a modified Latin alphabet for all
 *   Turkish public writing, backed by night-school campaigns (Millet
 *   Mektepleri), print licensing, and penalties for official old-script use.
 *   This file instantiates ONE reading of the contested kernel
 *   script_as_identity — the kemalist_rupture_reading, whose structural
 *   commitments are: zero transition cost (the reading recognizes no
 *   legitimate incumbents to displace), textual rupture as feature rather
 *   than bug, and a state monopoly over the literacy apparatus. The standing
 *   arrangement under contest — the enforced Latin-script regime and its
 *   enforcement machinery — is the fixed epsilon referent; the authored
 *   epsilon (0.20) is THIS reading's indexed assessment of that referent, far
 *   below what the payer seats compute from the same structure. Sibling
 *   files: script_as_identity__ottoman_continuity_reading (authors high
 *   epsilon for the same referent — statutory amputation of a constitutive
 *   identity bond) and script_as_identity__phonetic_instrumentalism_reading
 *   (authors near-zero epsilon — a neutral technology choice whose only costs
 *   are transition friction). Same referent, three epsilon values: the
 *   divergence is the kernel contest made measurable. Assumptions: the
 *   interval 1928-1950 captures the full enforcement lifecycle (mandate, peak
 *   campaign, liberalization-era decay); end-state scalars follow the
 *   endpoint convention of the worked example; the extractiveness series is
 *   reading-indexed (see logic_rationale). KEY AGENTS (by structural
 *   relationship): - kemalist_single_party_state: agenda-setter and primary
 *   collector (institutional/arbitrage) — wrote and enforced the mandate,
 *   collected the settlement's gains - ottoman_literate_officialdom: primary
 *   target (organized/trapped) — working literacy invalidated by statute -
 *   islamic_clerical_establishment: primary target
 *   (organized/identity_locked) — textual authority severed from its
 *   successor readership - transition_generation_adults: diffuse target
 *   (powerless/trapped) — bore the night-school and double-literacy burden -
 *   secular_republican_intelligentsia and latin_script_school_generation:
 *   principal beneficiaries (organized/mobile, moderate/mobile) -
 *   old_script_printers_and_exile_press and reformed_arabic_script_advocates:
 *   excluded seats — barred from the decision and the domestic market -
 *   foreign_turcology_observers: analytical observer — measured outcomes from
 *   outside the settlement
 *
 * KEY AGENTS:
 *   - kemalist_single_party_state: agenda-setter and primary collector (institutional/arbitrage) — enacted Law 1353, ran the literacy campaigns, licensed printing, and accrued the settlement's gains directly
 *   - ottoman_literate_officialdom: primary target (organized/trapped) — scribes, clerks, poets, historians whose script-specific human capital was invalidated by statute
 *   - islamic_clerical_establishment: primary target (organized/identity_locked) — medrese-trained scholars whose interpretive authority ran through the severed textual transmission
 *   - transition_generation_adults: diffuse target (powerless/trapped) — adults liable to attendance drives and fines, many left semi-literate in both scripts
 *   - republican_bureaucracy: beneficiary (institutional/constrained) — retooled administration onto the new standard; careers flow through the arrangement
 *   - secular_republican_intelligentsia: beneficiary (organized/mobile) — filled the new journals and textbooks as the old-script audience was legislated away
 *   - latin_script_school_generation: beneficiary (moderate/mobile) — acquired cheap literacy and inherited the settlement's entire upside
 *   - old_script_printers_and_exile_press: excluded (organized/arbitrage) — relocated presses abroad when old-script publication was restricted
 *   - reformed_arabic_script_advocates: excluded (moderate/constrained) — vowel-complete Arabic proposals foreclosed without trial by the 1928 law
 *   - foreign_turcology_observers: analytical observer (analytical/analytical) — measured literacy outcomes and debated the phonetic claims from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.2).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.44).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Kemalist Rupture Reading — Latin Script Mandate as Civilizational Severance").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/state-building/comparative-linguistics").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '66923ec3-5766-4036-8d47-a74e7ead0ab8').
narrative_ontology:cs_kernel_codification('66923ec3-5766-4036-8d47-a74e7ead0ab8', formalized).
narrative_ontology:cs_authority_grounding('66923ec3-5766-4036-8d47-a74e7ead0ab8', extraction).
narrative_ontology:cs_interpretation_layer_present('66923ec3-5766-4036-8d47-a74e7ead0ab8').
narrative_ontology:cs_reading_relation('66923ec3-5766-4036-8d47-a74e7ead0ab8', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('66923ec3-5766-4036-8d47-a74e7ead0ab8', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('66923ec3-5766-4036-8d47-a74e7ead0ab8', foundational, textual_severance_is_liberation).
narrative_ontology:cs_axiom_status(textual_severance_is_liberation, holdable).
narrative_ontology:cs_axiom_grounding('66923ec3-5766-4036-8d47-a74e7ead0ab8', textual_severance_is_liberation, instrumental).
narrative_ontology:cs_axiom('66923ec3-5766-4036-8d47-a74e7ead0ab8', foundational, state_literacy_monopoly_is_founding_prerogative).
narrative_ontology:cs_axiom_status(state_literacy_monopoly_is_founding_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('66923ec3-5766-4036-8d47-a74e7ead0ab8', state_literacy_monopoly_is_founding_prerogative, conventional).
narrative_ontology:cs_axiom('66923ec3-5766-4036-8d47-a74e7ead0ab8', secondary, old_script_residue_is_managed_transient).
narrative_ontology:cs_axiom_status(old_script_residue_is_managed_transient, holdable).
narrative_ontology:cs_axiom_grounding('66923ec3-5766-4036-8d47-a74e7ead0ab8', old_script_residue_is_managed_transient, conventional).
narrative_ontology:cs_reference_frame('66923ec3-5766-4036-8d47-a74e7ead0ab8', rupture_as_founding_frame).
narrative_ontology:cs_drift_state('66923ec3-5766-4036-8d47-a74e7ead0ab8', multiparty_liberalization_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('66923ec3-5766-4036-8d47-a74e7ead0ab8', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_single_party_state).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, republican_bureaucracy).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_republican_intelligentsia).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, latin_script_school_generation).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literate_officialdom).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, islamic_clerical_establishment).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, transition_generation_adults).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, script_civilizational_determinism).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, top_down_modernization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacted Law 1353 in November 1928 mandating the new Turkish letters for all public writing, funded the Millet Mektepleri night-school campaign, licensed printing, and staffed the new schools. Collected the settlement's gains directly: a uniform administrative script, a literacy apparatus answerable only to itself, and a public sphere cleared of rival textual authorities. Exit from the arrangement is meaningless for it — it wrote the rules and sits outside them.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_single_party_state, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, kemalist_single_party_state, beneficiary).

% Retooled record-keeping, courts, and provincial administration onto the new script within the mandated window; younger clerks advanced as Ottoman-script seniors retired. Careers, pensions, and standing flow through the arrangement it staffs; leaving would mean exiting the state itself.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, republican_bureaucracy, beneficiary,
    institutional, biographical, constrained, national).

% Writers, teachers, and lawyers whose Latin-script output filled the new journals and textbooks; their authority rose as the Ottoman-literate generation's audience was legislated away. Their skills were portable to European universities and exile circuits, giving them more room than most to simply leave.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_republican_intelligentsia, beneficiary,
    organized, generational, mobile, national).

% Cohorts schooled entirely in the new alphabet from the early 1930s onward; acquired literacy in months rather than years and entered state employment, the army, and the professions on the new standard. They bear little of the transition's cost and inherit its upside; their attachment to the arrangement is the settlement's intended product.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, latin_script_school_generation, beneficiary,
    moderate, biographical, mobile, national).

% Scribes, clerks, poets, and historians whose working literacy was invalidated by statute; their archives, correspondence, and libraries became inaccessible to their own children. Some retrained in night schools, many withdrew; their specialized capital had no domestic market after old-script publication was restricted.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literate_officialdom, payer,
    organized, biographical, trapped, national).

% Medrese-trained scholars and sermon-givers whose interpretive authority ran through Ottoman textual transmission; the reform cut their link to the coming generation of readers, and the closing of the medreses removed their institutional base. Their standing is fused with the textual tradition itself — abandoning it would dissolve the authority they hold — so they persist at the margin rather than exit. Their reference network spans the wider Muslim textual world, which is why their stake outruns the national border.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, islamic_clerical_establishment, payer,
    organized, civilizational, identity_locked, continental).

% Adults aged roughly twenty to fifty in 1928, liable to night-school attendance drives and exposed to penalties for official old-script use; many ended the campaign semi-literate in both scripts. They had no vote on the reform and no script-space outside the state's.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, transition_generation_adults, payer,
    powerless, biographical, trapped, national).

% Istanbul printing houses specializing in Ottoman titles and the emigre journals published in Cairo and elsewhere; barred from the domestic market as old-script publication was restricted, they relocated presses abroad and served readers they could no longer reach at home. They would argue for dual-script coexistence; their objection is audible only from outside the jurisdiction.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, old_script_printers_and_exile_press, excluded,
    organized, biographical, arbitrage, continental).

% Linguists and educators who proposed a vowel-complete reformed Arabic script through the early 1920s debates; their proposals were technically live until the 1928 law foreclosed them without trial. They would argue the coordination goal was achievable without the civilizational break; they were not seated in the committee that decided.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, reformed_arabic_script_advocates, excluded,
    moderate, generational, constrained, national).

% European and American scholars of Turkish and of literacy who measured the campaign's results, debated the phonetic claims, and compared Turkey with modernizing states that kept their inherited scripts. They take no side in the settlement and bear none of its costs; their seat is analytical.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, foreign_turcology_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, kemalist_single_party_state).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real coordination problem: a single, uniformly taught script for administration, schooling, and print across a population whose inherited script took years to master, enabling the mass-literacy campaign. Stated without evaluation: whatever its identity stakes, the arrangement did coordinate national literacy onto one standard.
% TRANSFER_FUNCTION: Moves textual authority and cultural capital from holders of Ottoman-script literacy (scribes, clergy, poets, archive users) to the state and the Latin-educated generation; it also moves the accessible past itself — pre-1928 writing becomes a specialist domain overnight.
% ABSENT_VOICES: Old-script literates had a vote in neither the drafting nor the rollout: the Grand National Assembly debated the law among the already-converted; village adults subject to attendance drives were not consulted; reformed-Arabic advocates were heard in the press but not in the committee; exile publishers could object only from Cairo. The unanimity of the record reflects who was seated.
% DISAPPEARANCE_RATIONALE: If the mandate and its machinery vanished overnight, every printed page, schoolbook, census form, and archive-access rule would lose its basis: administration, schooling, and publishing would face immediate dual-script crisis, the Latin-educated majority would be estranged from the state's own paper record, and whichever script won the ensuing scramble would re-found the literacy order — the arrangements built on the settlement depend on it.
% FOUNDING_PROBLEM: The new republic inherited a literacy bottleneck — an Arabic script ill-suited to Turkish vowel harmony that took years to master and capped mass education — and its founders wanted a citizenry oriented to the new state rather than to the Ottoman-Islamic textual order. The reform was built to solve both at once: cheap literacy plus a clean break.
% FOUNDING_PROBLEM_CORROBORATION: The literacy half is corroborated from outside the benefiting parties: foreign Turcology and literacy research of the 1930s-40s independently attested both the phonetic bottleneck and the campaign's gains. The rupture half is attested mainly by the regime's own cadres; later critics confirm the intent while reversing its valuation, but no source outside the beneficiary set attests that the severance itself was necessary rather than chosen — that absence is itself signal.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).
:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.20 is READING-INDEXED: the Kemalist seat assesses the arrangement it endorses, counting transition costs as founding investment and denying incumbent legitimacy to the displaced (its own delta: zero transition cost, no incumbents). The declining base_extractiveness series is therefore this reading's consolidation curve — perceived extraction falls as the rupture narrative solidifies — not a claim that descriptive extraction fell; the payer seats' computed chi stays high throughout, and the structural displacement compounds as cohort turnover renders the loss irreversible. Suppression 0.44 is the end-state of an honest ratchet-and-release: the suppression_requirement series peaks at 0.80 in 1931 (campaign expansion, publication bans hardening) and decays to 0.44 by 1950 as enforcement stands down — enforcement decay, not deregulation of the underlying mandate. Theater 0.36 rises as the functional transition completes and commemorative activity (anniversary rites, founder iconography) replaces it; the campaign itself was overwhelmingly functional early. Accessibility_collapse 0.55: alternatives (reformed Arabic script, dual-script coexistence) collapsed completely in official space but persisted privately and in exile publishing. Resistance 0.5: grumbling, passive noncompliance, exile journalism, clerical objection — but no organized reversal movement was feasible against a single-party state. Claim/metric independence: claimed_type tangled_rope is my structural read of the standing arrangement — a genuine coordination function (one phonetic standard demonstrably enabled mass literacy) fused with statutory asymmetric displacement (an entire literate class's capital confiscated) held by active enforcement. It is not tuned to the reading's self-image (which would claim rope) nor to the payer seats' computation (snare-leaning); the divergence is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The founder seat experiences the arrangement as pedagogy and founding: costly, deliberate, and self-justifying. The payer seats experience the same statutes as confiscation (officialdom), amputation (clerical establishment), and conscription (transition adults). The beneficiary seats experience opportunity. The engine computes per-seat classifications from the structural data; the authored epsilon pins the reading's own seat at 0.20, and the gap between that figure and the payer seats' computed chi is precisely what this file contributes to the kernel comparison. No coalition dynamics rescue the powerless seat here: the transition generation was atomized by geography and by the state's monopoly on schooling, and its natural allies (old-script literates) were themselves the constraint's primary targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the state sits nearest the beneficiary pole (arbitrage exit atop its agenda-setting position — it wrote the rules and stands outside them), with the bureaucracy, intelligentsia, and school generation damped progressively less by their weaker exits. Victim declarations drive high d: the clerical establishment sits nearest the full-target end because identity_lock removes exit entirely — abandoning the textual tradition dissolves the authority it holds; trapped officialdom and trapped transition adults sit just behind it. The excluded seats (exile press, reformed-Arabic advocates) are commentary-grade per R3 and receive no correction-grade directionality overrides; their position is recorded in their situations and in absent_voices, not in the derivation chain. Observers are analytical and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: a literacy bottleneck (Arabic script's poor fit to Turkish vowel harmony capped mass education) and a civilizational orientation problem (the founders wanted a citizenry oriented to the new state rather than the Ottoman-Islamic textual order). The literacy half is dead — solved within a generation and corroborated by outside measurement. The orientation half is contested: this reading holds it permanently live (rupture as continuing constitution), critics hold it settled-then-reversed. Founding_problem_status is therefore authored 'contested', and the mismatch consumer finds no dead-problem zombie signature against the world_rearranges verdict — but the contested status records the live dispute rather than resolving it. The tangled_rope classification prevents both symmetrical failures: a pure-snare label would erase the real literacy coordination the campaign delivered to millions; a pure-rope label would erase the statutory confiscation of a literate class and the state's capture of the literacy apparatus. The reading's own low epsilon marks where this file sits inside that contest, not where the arrangement objectively lands.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the kemalist_rupture_reading of kernel script_as_identity; do the sibling readings (ottoman_continuity_reading, phonetic_instrumentalism_reading) assign structurally different epsilon, victim sets, and types to the same standing arrangement?',
    'Cross-file comparison of the three reading-stories over the shared referent (the enforced Latin-script regime); divergence in authored epsilon and victim sets confirms the kernel is genuinely contested rather than one constraint mislabeled.',
    'If the siblings converge on this reading''s profile, the kernel collapses to a single constraint and this file''s reading-indexed epsilon becomes a general estimate; if they diverge as expected, per-seat classification must be read reading-relative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas route here rather than into standard fields.').

omega_variable(
    severance_causal_necessity,
    'Was severing the Arabic-script textual inheritance causally necessary for Turkish secular modernization, or did modernization proceed despite or independent of the rupture?',
    'Comparative-historical analysis against modernizing states that retained or partially reformed their inherited scripts (for example Japan''s mixed-script modernization), controlling for state capacity and education spending.',
    'Confirmed necessity grounds the reading''s instrumental axiom; refuted necessity re-weights the arrangement''s extraction toward pure displacement of incumbents and undermines textual_severance_is_liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(severance_causal_necessity, empirical, 'Whether the rupture mechanism the reading depends on actually did the causal work it claims.').

omega_variable(
    incumbent_legitimacy_ledger,
    'The reading''s structural delta claims zero transition cost because no legitimate incumbents existed to displace; does the ledger of displaced Ottoman-script literates, clergy, and transition-generation adults support that denial?',
    'Archival reconstruction of displaced-official outcomes, medrese closure records, and literacy-campaign attendance versus attrition.',
    'A substantively nonzero legitimate-incumbent cost converts the reading''s low authored epsilon into systematic understatement and shifts computed classification toward the payer seats'' verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_legitimacy_ledger, empirical, 'Tests the reading''s definitional move of pricing the displaced out of the cost ledger.').

omega_variable(
    neutrality_construal_scope,
    'Is the instrumentalist sibling''s premise (script is neutral technology) strictly incompatible with this reading''s severance mechanism, or compatible under a construal on which scripts are intrinsically neutral and identity-loading is community-projected?',
    'Conceptual analysis fixing the scope of the neutrality claim (intrinsic valence versus projected valence) and re-testing the foreclosure edge to phonetic_instrumentalism_reading.',
    'Under the projected-valence construal the relation relaxes from foreclosure toward coexistence and the kernel''s contest structure changes shape; under the intrinsic-valence construal the declared foreclosure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_construal_scope, conceptual, 'CS-framing under-determination: the neutrality claim''s scope decides whether the instrumentalist sibling is foreclosed or merely pressured.').

omega_variable(
    consolidation_vs_ratchet,
    'Did the arrangement''s persistence after enforcement decay reflect consolidated consent among the Latin-educated majority, or a ratchet in which cohort turnover made reversal impossible regardless of preference?',
    'Survey and electoral evidence from the 1950s onward on script attitudes cross-cut by cohort, plus counterfactual analysis of reversion feasibility.',
    'Consolidated consent supports the coordination half of the tangled-rope structure; pure ratchet shifts weight toward enforced path dependence and raises the standing arrangement''s effective extraction for later cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consolidation_vs_ratchet, empirical, 'Distinguishes legitimate consolidation from lock-in after the enforcement machinery stood down.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.18).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1928, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1931, script_as_identity__kemalist_rupture_reading, theater_ratio, 1931, 0.2).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1931, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1934, script_as_identity__kemalist_rupture_reading, theater_ratio, 1934, 0.23).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1934, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1938, script_as_identity__kemalist_rupture_reading, theater_ratio, 1938, 0.26).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1938, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1943, script_as_identity__kemalist_rupture_reading, theater_ratio, 1943, 0.29).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1943, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1946, script_as_identity__kemalist_rupture_reading, theater_ratio, 1946, 0.33).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1946, observed).
narrative_ontology:measurement(kemalist_rupture_reading_tr_t1950, script_as_identity__kemalist_rupture_reading, theater_ratio, 1950, 0.36).
narrative_ontology:measurement_basis(kemalist_rupture_reading_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(kemalist_rupture_reading_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.42).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1928, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1931, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1931, 0.36).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1931, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1934, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1934, 0.31).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1934, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1938, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1938, 0.27).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1938, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1943, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1943, 0.24).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1943, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1946, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1946, 0.22).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1946, observed).
narrative_ontology:measurement(kemalist_rupture_reading_be_t1950, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement_basis(kemalist_rupture_reading_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(kemalist_rupture_reading_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.78).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1928, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1931, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1931, 0.8).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1931, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1934, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1934, 0.76).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1934, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1938, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1938, 0.7).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1938, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1943, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1943, 0.62).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1943, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1946, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1946, 0.52).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1946, observed).
narrative_ontology:measurement(kemalist_rupture_reading_su_t1950, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1950, 0.44).
narrative_ontology:measurement_basis(kemalist_rupture_reading_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, information_standard).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Turkish script reform' decomposes, per the epsilon-invariance principle, into three structurally distinct claims sharing one referent (the enforced Latin-script regime). The continuity reading authors high epsilon (amputation of a constitutive bond); this rupture reading authors low reading-indexed epsilon (endorsed founding surgery); the instrumentalist reading authors near-zero epsilon (neutral technology choice). Each file carries its own beneficiaries, victims, axioms, and drift state; the upstream rupture reading structurally influenced the downstream instrumentalist reading by supplying its public justification. Downstream of the whole family sits the 1930s language-purification campaign, which inherits the rupture's clean-slate logic and is a candidate future node in this network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
