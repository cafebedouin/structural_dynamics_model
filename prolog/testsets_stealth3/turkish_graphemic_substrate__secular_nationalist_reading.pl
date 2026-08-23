% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Secular-Nationalist Latin Graphemic Substrate Mandate (1928 Alphabet Reform)
 *   domain: political linguistics / state formation / cultural engineering
 *
 * SUMMARY:
 *   In November 1928 the Turkish Grand National Assembly adopted a modified
 *   Latin alphabet and criminalized Arabic-script typesetting in official and
 *   published life, completing a rupture the republican leadership had
 *   prepared since 1923. The arrangement under contest — the standing one
 *   this story is about — is the resulting regime: a single state-mandated
 *   graphemic substrate, enforced through schools, press licensing, and penal
 *   provision, that severed the new citizenry's written access to the
 *   Ottoman-Islamic archive while delivering a genuine mass-literacy
 *   capability. This file instantiates ONE reading of the
 *   turkish_graphemic_substrate kernel (see kernel_context); the epsilon
 *   authored here is indexed to that reading's own lights over the standing
 *   arrangement, never averaged across readings. KEY AGENTS (by structural
 *   relationship): - republican_state_apparatus: agenda-setter
 *   (institutional/mobile) — wrote the law, runs enforcement, collects
 *   legibility and cultural authority - secularist_republican_elites: primary
 *   beneficiary (powerful/identity_locked) — careers and identity fused with
 *   the settlement - post_reform_school_generations: beneficiary with payer
 *   residue (organized/identity_locked) — gained literacy, lost the archive -
 *   ottoman_literate_generation: primary target (moderate/trapped) —
 *   functional illiteracy imposed overnight - islamic_scholarship_networks:
 *   target (organized/trapped) — textual authority severed -
 *   arabic_script_press: target (moderate/trapped) — converted or closed -
 *   rural_anatolian_communities: target with beneficiary residue
 *   (powerless/trapped) - european_diplomatic_partners: incidental
 *   beneficiary (institutional/arbitrage) - kurdish_language_communities:
 *   excluded seat (powerless/trapped) — an objecting voice with no address -
 *   historians_of_language_policy: analytical observer
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.42).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.34).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Secular-Nationalist Latin Graphemic Substrate Mandate (1928 Alphabet Reform)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political linguistics / state formation / cultural engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '75012fb0-62e2-47f3-add5-7b9f77ce6f51').
narrative_ontology:cs_kernel_codification('75012fb0-62e2-47f3-add5-7b9f77ce6f51', formalized).
narrative_ontology:cs_authority_grounding('75012fb0-62e2-47f3-add5-7b9f77ce6f51', extraction).
narrative_ontology:cs_interpretation_layer_present('75012fb0-62e2-47f3-add5-7b9f77ce6f51').
narrative_ontology:cs_reading_relation('75012fb0-62e2-47f3-add5-7b9f77ce6f51', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('75012fb0-62e2-47f3-add5-7b9f77ce6f51', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('75012fb0-62e2-47f3-add5-7b9f77ce6f51', foundational, turkish_identity_discontinuous_with_ottoman_islam).
narrative_ontology:cs_axiom_status(turkish_identity_discontinuous_with_ottoman_islam, holdable).
narrative_ontology:cs_axiom_grounding('75012fb0-62e2-47f3-add5-7b9f77ce6f51', turkish_identity_discontinuous_with_ottoman_islam, deontological).
narrative_ontology:cs_axiom('75012fb0-62e2-47f3-add5-7b9f77ce6f51', foundational, latin_script_sole_legitimate_graphic_substrate).
narrative_ontology:cs_axiom_status(latin_script_sole_legitimate_graphic_substrate, holdable).
narrative_ontology:cs_axiom_grounding('75012fb0-62e2-47f3-add5-7b9f77ce6f51', latin_script_sole_legitimate_graphic_substrate, instrumental).
narrative_ontology:cs_axiom('75012fb0-62e2-47f3-add5-7b9f77ce6f51', secondary, state_guardianship_of_linguistic_revolution).
narrative_ontology:cs_axiom_status(state_guardianship_of_linguistic_revolution, holdable).
narrative_ontology:cs_axiom_grounding('75012fb0-62e2-47f3-add5-7b9f77ce6f51', state_guardianship_of_linguistic_revolution, conventional).
narrative_ontology:cs_reference_frame('75012fb0-62e2-47f3-add5-7b9f77ce6f51', european_aligned_secular_modernity).
narrative_ontology:cs_drift_state('75012fb0-62e2-47f3-add5-7b9f77ce6f51', contemporary_neo_ottoman_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('75012fb0-62e2-47f3-add5-7b9f77ce6f51', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secularist_republican_elites).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, post_reform_school_generations).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_diplomatic_partners).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_generation).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, islamic_scholarship_networks).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, arabic_script_press).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_communities).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, post_reform_school_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislated the alphabet in November 1928, criminalized Arabic-script typesetting in official and published domains, and built the Millet Mektepleri national schools to re-teach the adult population. Collects administrative legibility, a conscription-ready literate populace, and centralized cultural authority. Writes and amends the rules; reversal is available in principle but would dissolve its own founding credential.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Ministry cadres, officers, jurists, and university faculty whose political identity is fused with the reform project. The script settlement vindicates their claim to speak for a European-facing nation; their careers, networks, and self-concept are constituted inside it. Leaving would mean renouncing the republican identity they administer.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secularist_republican_elites, beneficiary,
    powerful, biographical, identity_locked, national).

% Educated exclusively in the Latin alphabet from the first reformed cohorts onward. Gained faster literacy acquisition and entry into the modernizing economy; cannot read grandparents' letters, Ottoman archives, or pre-1928 books without special training. Locked in because their entire literate life runs on the Latin substrate.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, post_reform_school_generations, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, post_reform_school_generations, payer).

% European states and institutions that gained a western-oriented partner in trade, diplomacy, and eventually alliance structures. They bear none of the enforcement costs and can redirect engagement elsewhere at will; their benefit is incidental to the settlement's internal politics.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_diplomatic_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Adults already literate in Ottoman script when the law landed. Overnight their literacy ceased to count in offices, courts, and newspapers; many never retrained and withdrew from public written life. Private reading and correspondence persisted, but career paths and civic participation closed. Exit would have required becoming students again in middle age, under teachers younger than themselves.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_generation, payer,
    moderate, biographical, trapped, national).

% Ulema, medrese-trained scholars, and Sufi orders whose authority rested on mastery of Arabic-script texts. With the medreses closed (1924) and the script ban in force, their public voice was severed and their libraries became unreadable to the young. Their institutional base spans the wider former Ottoman ecumene, but inside the republic they had no channel to contest the settlement.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, islamic_scholarship_networks, payer,
    organized, generational, trapped, continental).

% Newspapers, journals, and printers working in Ottoman script faced an immediate choice: absorb the cost of new typefaces, retrained compositors, and lost subscribers, or close. Many folded; the plural Ottoman public sphere contracted into a single licensed print culture aligned with the state.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, arabic_script_press, payer,
    moderate, immediate, trapped, national).

% Conscripted into the literacy campaigns as enrollees and hosts; bore the disruption of re-schooling with the least compensation. Were the declared intended beneficiaries of mass literacy, yet were consulted only as enrollment statistics; village transmission chains between literate elders and the young broke.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_communities, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, rural_anatolian_communities, beneficiary).

% The national script settlement recognized Turkish alone. Kurdish-speaking citizens had no seat in the deliberations and, for decades, no legal way to publish in their own language in any script. They would have objected to a homogenizing substrate that treated linguistic plurality as a threat; their objection had no institutional address.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_language_communities, excluded,
    powerless, generational, trapped, regional).

% Comparative scholars studying the 1928 reform alongside contemporaneous Soviet latinization and other twentieth-century script-engineering episodes. They see both the genuine coordination achievement and the coercive severance, and hold no stake in either reading's victory.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, historians_of_language_policy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes one phonetically adequate national writing system: Latin letters represent Turkish vowel harmony far better than the Arabic abjad, cutting literacy-acquisition time and unifying schooling, printing, and administration across the successor territory.
% TRANSFER_FUNCTION: Moves textual authority and public voice from Ottoman-Islamic learned classes to the republican state and its Latin-educated citizenry; moves compliance labor and attention from the whole population into state-directed literacy campaigns; moves cultural capital — access to the pre-1928 archive — away from anyone not retrained.
% ABSENT_VOICES: Ottoman-literate scholars and the Arabic-script press held no seat once the law passed — parliamentary opponents were outmaneuvered and marginalized. Kurdish-speaking communities were never in the room at all. The unanimity recorded in 1928 arose partly because the seats that would have objected had already been emptied or never existed.
% DISAPPEARANCE_RATIONALE: Every layer of written Turkish life — schooling, bureaucracy, publishing, and now digital input methods — presupposes the Latin substrate. Overnight removal would strand every living literate Turk, force a second forced migration between scripts, and reorganize the state's administrative and pedagogical machinery around whichever replacement won.
% FOUNDING_PROBLEM: Mass illiteracy (roughly ninety percent) under an Arabic abjad poorly fitted to Turkish phonology, compounded by a state-building project that wanted a distinct national identity severed from the Ottoman-Islamic past and aligned with European civilization.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set by the Dewey report on Turkish education (1924), comparative literacy statistics later compiled by UNESCO, and European diplomatic archives recording the alignment motive. Ottoman-continuity partisans — outside the beneficiary set — dispute the diagnosis itself, attesting that the identity problem was manufactured and the literacy problem solvable without rupture; their live dissent is itself evidence the status is contested rather than settled.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).
:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.42 from THIS reading's seat: the reading assesses the standing arrangement as its own legitimate modernization project, so the real costs (generational severance, coerced conversion) register as heavy-but-necessary transition prices rather than as rent — a continuity reading of the same referent would author markedly higher epsilon. Suppression (0.34) is a raw structural property, deliberately unscaled; it reflects the enforcement picture at interval end, where legal penalties have lapsed but the script monopoly in official life remains total. Theater (0.35) rises slowly: commemorations, official rhetoric, and alphabet anniversaries thicken around a function that now largely sustains itself. The temporal series run on ONE shared nine-point grid — all three metrics authored at every point — so no end-state value is backfilled into earlier rows. The mild late-interval oscillation in extraction and theater tracks Turkey's recurring secularist/revivalist political cycles (the 1960/1971/1980 intervention waves tightening enforcement salience; post-2002 revival politics raising it again); the oscillation is a side effect of external identity politics, not itself the extraction mechanism. Accessibility collapse (0.68) is high but incomplete: Arabic-script publication survives in religious niches and Ottoman-script study is legal, yet no practical alternative to the Latin substrate exists for civic life. Resistance (0.30) is the residue of a resistance that was once fierce and was broken early. Coordination type is authored as identity_coordination because THIS reading's dominant function is boundary maintenance of national identity against the Islamic past — the encoding function is real, but the reading's distinctive claim is legitimacy and alignment; the FNL gaming alert is answered by noting the membership coordination (schooling, citizenship ritual) is performed, not merely narrated, while the asymmetric costs are carried by the declared victims.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement computes as coordination the state built and pays to maintain; from the trapped payer seats (the 1928-literate adults, the ulema, the converted-or-closed press) the same structure computed as confiscation of their literate life. The post-reform cohorts straddle the gap: net beneficiaries by literacy outcome, payers by heritage access, and identity-locked in both directions — their lock is professional-educational (career and daily literacy run on Latin) fused with national identity (the alphabet is the republic's emblem). The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (state apparatus, secularist elites, post-reform cohorts, European partners) drive those seats toward the subsidy end; victim declarations (the Ottoman-literate generation, the scholarship networks, the press, rural communities) drive them toward the full-target end, with trapped exit pushing them nearer the target pole than mobile targets would sit. European partners derive near-beneficiary directionality despite institutional power because they bear no enforcement cost and hold arbitrage-grade exit. The post-reform cohorts carry a secondary payer role the derivation partially registers; their net position stays beneficiary-side because the literacy gain dominates their own welfare accounting — the intergenerational-accounting omega holds that contest open rather than resolving it by fiat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — move the nation out of Ottoman script — was accomplished within roughly a generation; what persists is maintenance of an achieved state, which is why mandatrophy_resolved is declared true. The classification guards against two mislabels: reading the arrangement as a snare would erase the genuine coordination achievement (mass literacy in a phonologically fitted script); reading it as a pure rope would erase the identifiable people who paid through the same structure. Founding_problem_status is authored 'contested' rather than 'dead' precisely so the mismatch consumer judges a dispute the parties genuinely hold, instead of firing a zombie flag off an author's assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (secular_nationalist) of the turkish_graphemic_substrate kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three sibling stories: if ottoman_continuity_reading computes continuity-preserving arrangements as low-extraction, or gradual_transition_reading computes managed dual-literacy as feasible and cheap, the exclusivity premise of this reading loses its necessity claim.',
    'Sibling readings instantiate different victim sets (the continuity reading severs the Latin-educated cohort from heritage; the gradual reading severs neither cohort fully) and different epsilon over the same referent; this story''s classification is invariant to their outcomes, but corpus-level comparison tests whether the rupture was necessary or merely chosen.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in the identity-continuity premise and the ground of script legitimacy.').

omega_variable(
    infrastructure_irreversibility_question,
    'Has the Latin substrate become self-sustaining infrastructure (mountain-like irreversibility) or does it remain dependent on continued state enforcement of the rupture narrative?',
    'Deregulation counterfactual: observe whether Arabic-script publication and education expand where legal barriers have been lifted (post-1950 evidence shows partial expansion in religious niches and no mainstream return).',
    'If irreversible, remaining suppression is vestigial and the constraint trends toward rope-like coordination; if reversible, the enforcement load is load-bearing and the extraction assessment rises for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_irreversibility_question, empirical, 'Whether the graphemic settlement is now self-enforcing infrastructure or still enforcement-dependent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the residual suppression keeping Ottoman-script literacy marginal structural (no institutional channels) or internalized (the rupture norm carried by the population itself)?',
    'Post-liberalization uptake trajectory: Ottoman-script courses have been legal and even state-offered since the 1950s; if uptake stays negligible despite availability, suppression is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the elder-generation exclusion persists without enforcement; the effect concentrates on the payer seats'' exit assessments rather than on the aggregate scalar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism behind the script monopoly.').

omega_variable(
    intergenerational_benefit_accounting,
    'Are the post-reform cohorts net beneficiaries (fast literacy, European integration) or net victims (severed from the textual heritage of their own grandparents)?',
    'Welfare comparison across cohorts at matched education levels, plus revealed preference: voluntary Ottoman-script course enrollment among Latin-educated adults.',
    'Flips the largest cohort seat between beneficiary-side and payer-side directionality; determines whether the coordination function''s gains reach the coordinated or are captured upstream by the state and its elites.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_benefit_accounting, conceptual, 'Framing-dependent valuation of the reform''s intergenerational ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 97).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgs_secular_nat_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(tgs_secular_nat_tr_t12, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement(tgs_secular_nat_tr_t24, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement(tgs_secular_nat_tr_t36, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 36, 0.24).
narrative_ontology:measurement(tgs_secular_nat_tr_t48, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 48, 0.27).
narrative_ontology:measurement(tgs_secular_nat_tr_t60, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 60, 0.29).
narrative_ontology:measurement(tgs_secular_nat_tr_t72, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 72, 0.31).
narrative_ontology:measurement(tgs_secular_nat_tr_t84, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 84, 0.33).
narrative_ontology:measurement(tgs_secular_nat_tr_t97, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 97, 0.35).

% Extraction over time
narrative_ontology:measurement(tgs_secular_nat_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(tgs_secular_nat_be_t12, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(tgs_secular_nat_be_t24, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(tgs_secular_nat_be_t36, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 36, 0.43).
narrative_ontology:measurement(tgs_secular_nat_be_t48, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 48, 0.41).
narrative_ontology:measurement(tgs_secular_nat_be_t60, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(tgs_secular_nat_be_t72, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 72, 0.41).
narrative_ontology:measurement(tgs_secular_nat_be_t84, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 84, 0.42).
narrative_ontology:measurement(tgs_secular_nat_be_t97, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 97, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tgs_secular_nat_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(tgs_secular_nat_su_t12, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(tgs_secular_nat_su_t24, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(tgs_secular_nat_su_t36, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 36, 0.5).
narrative_ontology:measurement(tgs_secular_nat_su_t48, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 48, 0.44).
narrative_ontology:measurement(tgs_secular_nat_su_t60, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 60, 0.39).
narrative_ontology:measurement(tgs_secular_nat_su_t72, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 72, 0.36).
narrative_ontology:measurement(tgs_secular_nat_su_t84, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 84, 0.35).
narrative_ontology:measurement(tgs_secular_nat_su_t97, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 97, 0.34).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate__ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate__gradual_transition_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_language_purification).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Turkish script reform' decomposes into a three-reading kernel family per the epsilon-invariance principle: this secular-nationalist instantiation (rupture, Latin exclusivity), an ottoman-continuity sibling (anti-rupture, Arabic legitimacy), and a gradual-transition sibling (managed dual literacy). Each is a separate constraint story with its own epsilon over the same standing arrangement; the secular reading sits upstream, having changed the legitimacy conditions and resource availability under which the other two operate. turkish_language_purification (the 1930s word-purification drive) is a downstream dependent drawing its warrant from this reading's distinctness premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
