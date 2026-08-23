% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Diaspora Bridge-Channel Hebrew (Contact-Language Reading)
 *   domain: sociolinguistics/commitment_systems
 *
 * SUMMARY:
 *   For roughly thirteen centuries after Hebrew ceased to be a widespread
 *   home vernacular, dispersed Jewish communities kept a single written
 *   channel alive: a high register of formulaic correspondence, legal
 *   responsa, and contract drafting, beside a looser marketplace register
 *   visible in trade letters and dictated family business. Native speakers
 *   thinned to pockets; the language's occupation of daily life ran through
 *   utility — merchants needed one mailbag-legible code across the
 *   Mediterranean and Indian-Ocean circuits, communities needed a responsa
 *   loop, charities needed remittance rails, courts needed a fixed contract
 *   language. Access was sold and rationed: scribes charged per document,
 *   communal levies funded the schools that produced literacy, women and the
 *   unlettered reached the channel only through paid mediators, and a
 *   scholarly minority defected to Judeo-Arabic for subjects the inherited
 *   vocabulary served badly. This story authors that standing arrangement as
 *   ONE reading of the hebrew_continuity kernel — the reading counting
 *   instrumental cross-communal use as the language's life — with its own
 *   stable epsilon and its own beneficiary/victim structure; sibling readings
 *   of the kernel are separate constraint stories joined in
 *   network.affects_constraints (see commentary.kernel_context). Agents, by
 *   structural relationship: the scribal-rabbinical estate administers and
 *   collects; communal leadership coordinates through the corridor;
 *   long-distance merchants buy reach they cannot otherwise obtain; teachers
 *   reproduce the input side; householders and women fund and endure mediated
 *   access; vernacular intellectuals stand outside; a modern comparativist
 *   observes.
 *
 * KEY AGENTS:
 *   - rabbinic_scribal_elites — agenda setter and collector seat ([institutional]/[identity_locked]) — staffs the channel, fixes document-language norms, and receives fees, appointments, and stipends
 *   - communal_leadership — coordinating beneficiary ([institutional]/[mobile]) — runs remittances and cross-border dispute forwarding, partially substitutable by court-language channels
 *   - long_distance_merchant_correspondents — paying beneficiary ([organized]/[constrained]) — buys cross-network communicability; bears courier, scribal, and schooling costs
 *   - hebrew_pedagogues — reproduction-side beneficiary ([moderate]/[constrained]) — supplies the literacy the channel consumes
 *   - non_literate_laity — primary target seat ([powerless]/[trapped]) — funds the channel through levies, accesses it only via paid mediation
 *   - diaspora_women — primary target seat ([powerless]/[trapped]) — barred from schooling, transacts exclusively through mediators
 *   - vernacular_science_writers — excluded defector seat ([powerful]/[arbitrage]) — routes intellectual production around the channel
 *   - sociolinguistic_comparativist — analytical observer ([analytical]/[analytical]) — sees the full two-register structure from documentary corpora
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.38).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.26).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.38).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Diaspora Bridge-Channel Hebrew (Contact-Language Reading)").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '621dcce9-928f-4bf8-a98e-258414e950f0').
narrative_ontology:cs_kernel_codification('621dcce9-928f-4bf8-a98e-258414e950f0', distributed).
narrative_ontology:cs_authority_grounding('621dcce9-928f-4bf8-a98e-258414e950f0', practice).
narrative_ontology:cs_interpretation_layer_present('621dcce9-928f-4bf8-a98e-258414e950f0').
narrative_ontology:cs_reading_relation('621dcce9-928f-4bf8-a98e-258414e950f0', hebrew_continuity__liturgical_preservation, coexists_with).
narrative_ontology:cs_reading_relation('621dcce9-928f-4bf8-a98e-258414e950f0', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_axiom('621dcce9-928f-4bf8-a98e-258414e950f0', foundational, intercommunal_use_suffices_for_continuity).
narrative_ontology:cs_axiom_status(intercommunal_use_suffices_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('621dcce9-928f-4bf8-a98e-258414e950f0', intercommunal_use_suffices_for_continuity, empirically_contingent).
narrative_ontology:cs_axiom('621dcce9-928f-4bf8-a98e-258414e950f0', secondary, register_flexibility_doctrine).
narrative_ontology:cs_axiom_status(register_flexibility_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('621dcce9-928f-4bf8-a98e-258414e950f0', register_flexibility_doctrine, conventional).
narrative_ontology:cs_reference_frame('621dcce9-928f-4bf8-a98e-258414e950f0', instrumental_channel_continuity).
narrative_ontology:cs_drift_state('621dcce9-928f-4bf8-a98e-258414e950f0', late_medieval_vernacular_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('621dcce9-928f-4bf8-a98e-258414e950f0', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, rabbinic_scribal_elites).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, long_distance_merchant_correspondents).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, communal_leadership).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, hebrew_pedagogues).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, non_literate_laity).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, diaspora_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, long_distance_merchant_correspondents).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, instrumental_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__bridge_pidginized, cross_diaspora_communicability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose the responsa that answer legal queries from distant communities, draft and validate marriage contracts, divorce bills, and court protocols in the prescribed language, train the next generation of scribes, and fix the epistolary conventions all correspondents follow. Scribal fees, court appointments, and academy stipends funded by communal levies flow to this estate; the trade passes within families, so leaving it forfeits inherited standing.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, rabbinic_scribal_elites, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, rabbinic_scribal_elites, beneficiary).

% Nagids, exilarchs, and elected heads run cross-border charity remittances to the academies and the Jerusalem poor, forward and settle custody and commercial disputes between communities, and address petition letters to rulers. Their reach extends as far as the letter channel does. For dealings with imperial courts they use the court language instead, which keeps their reliance on the Hebrew channel partial.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, communal_leadership, beneficiary,
    institutional, generational, mobile, continental).

% Merchants trading across the Mediterranean and Indian-Ocean circuits whose partners share no native tongue; accounts, orders, prices, and credit news travel in a single script and phrase-stock regardless of each writer's spoken language. They pay courier fees, scribe wages, and the schooling that produced their literacy, and walking away forfeits a correspondent web of trust built over decades.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, long_distance_merchant_correspondents, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, long_distance_merchant_correspondents, payer).

% Elementary instructors and copyists paid from communal education funds and household fees to teach boys the alphabet, prayer-book Hebrew, and letter-writing formulas. Their livelihood exists because every community obliges parents to educate sons; vernacular school initiatives shrink their market wherever they take root.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, hebrew_pedagogues, beneficiary,
    moderate, biographical, constrained, local).

% Householders who fund schools, scribes, and couriers through communal taxes, sign nothing themselves, and reach the written channel only by hiring a scribe or dictating to a literate neighbor. Marriage contracts, divorce proceedings, petitions, and inheritance claims enter the record mediated and fee-bearing. Standing outside the communal institutions that operate the channel means losing burial society, dowry funds, and dispute arbitration.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, non_literate_laity, payer,
    powerless, biographical, trapped, national).

% Barred from the elementary schools nearly everywhere, they transact family business — marriage settlements, divorce, remittances, property claims — through husbands, sons, brothers, or paid scribes. Surviving dictated letters show constant use of the channel by people never taught to write in it; each use carries a mediator's discretion and a fee.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_women, payer,
    powerless, biographical, trapped, regional).

% Physicians, astronomers, and philosophers who judge Hebrew's inherited vocabulary unfit for their subjects and publish for the educated public in Judeo-Arabic, and later in European vernaculars. They neither defend the Hebrew channel nor attack it; they route around it, and the prestige of their work makes the defection conspicuous.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, vernacular_science_writers, excluded,
    powerful, generational, arbitrage, continental).

% Modern analyst of the Cairo Geniza, responsa corpora, and manuscript colophons; sees the full two-register operation — formulaic high correspondence beside colloquial marketplace letters — and holds no stake in any historical party's position.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, sociolinguistic_comparativist, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives communities speaking mutually unintelligible vernaculars (Greek, Aramaic, Judeo-Arabic varieties, later Yiddish- and Romance-adjacent speech) one written channel for correspondence, trade accounts, legal queries and answers, charity logistics, and communal negotiation, so any two dispersed communities can communicate without arranging per-exchange translation.
% TRANSFER_FUNCTION: Moves scribal fees, courier charges, and school levies from households and merchants to the trained literate estate; moves network access and voice in written deliberation from the unlettered to those who can pay or mediate; moves legal rulings, credit news, and charitable funds along the corridor between communities.
% ABSENT_VOICES: Women and non-literate householders appear in the record chiefly as subjects — parties to marriage, divorce, and debt documents drafted for them — not as correspondents; their objections to fees and mediation survive mostly as marginalia and occasional dictated complaint. Poor levy-payers had no seat in communal budget councils. Vernacular-writing intellectuals removed themselves by choice and made their case in another language.
% DISAPPEARANCE_RATIONALE: Overnight loss of the channel severs the responsa loop (queries out, rulings back), breaks academy and Jerusalem charity remittances, strands merchants without their credit-news web, and forces every cross-community matter onto ad-hoc interpreter chains; regional clusters reorganize around local vernaculars and the Aramaic residue, and communal offices that exist to run the corridor lose their function.
% FOUNDING_PROBLEM: After Hebrew ceased to be a widespread home vernacular and the population dispersed across Greek-, Aramaic-, Arabic-, and later European-speaking empires, the communities needed a shared medium for law, commerce, mutual aid, and doctrinal coordination that no single local vernacular and no per-exchange translation arrangement could reliably supply.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary estate: Cairo Geniza merchants' letters show pragmatic channel selection (Arabic locally, Hebrew/Aramaic across the network) attesting real demand; Geonic responsa collections attest sustained query volume; the Judeo-Arabic scientific corpus attests the channel's limits from the defectors' side. No party's self-report is relied upon. On status: rabbinic elites attested liveness throughout the interval; the documentary tail — the shrinking Hebrew-letter share of Geniza traffic after c. 1200 and the rise of vernacular internal channels — supports the contested verdict at the interval's end.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).
:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.38 for the standing bridge arrangement as THIS reading assesses it: the referent is the channel arrangement itself, valued by the reading's own lights (instrumental utility is what counts as life here, so genuine service delivery caps extraction well below snare territory), while gatekeeping costs — per-document scribal fees, schooling levies, mediated access for women and the unlettered — keep it far above rope floor. Suppression 0.26 reflects real but bounded coercion: communal schooling obligations, standardized contract-language rules, and courier-funding duties, against always-available partial exits (hire a scribe, use the Aramaic parallel channel, write Arabic). Suppression is authored as a raw structural property and is never scaled by power or scope; only extractiveness is scaled downstream by the engine. Theater 0.30: epistolary formulae are load-bearing early and increasingly ornamental late. Accessibility collapse 0.45 and resistance 0.40 record that alternatives persist (interpreters, Aramaic, Judeo-Arabic scholarly defection) and met real pushback (vernacular intellectual flight, rival channels' pull). The claim is authored independently of the metrics: tangled_rope is asserted because the structure possesses BOTH a genuine coordination function (one written channel across mutually unintelligible vernaculars) AND asymmetric extraction (a literate estate collecting from a largely excluded base), held together by active enforcement — not because any metric was tuned to produce that verdict. Receipt surface: the arrangement's surplus demonstrably accrues to the scribal-rabbinical estate (fees, appointments, stipends), so gain_flow names that seat; the seat positioned to widen access is that same estate, for which opening the channel dissolves its rent base — fixing_cost prohibitive. Temporal series share one grid (centuries t=0..13, c. 200-1500 CE): extraction rises with scribal professionalization and network monetization, peaks c. 1000-1200, and recedes as vernaculars and print reroute traffic; suppression_requirement is tracked because the narrative genuinely traces enforcement-capacity change — institutional hardening (academy-funding ordinances, epistulary standardization) followed by decay; theater climbs monotonically as formula accumulates faster than function. The arcs are secular, not oscillatory: no intermittent-reinforcement cycle is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as an institution it staffs and reproduces — training, ordination, court appointment — and computes coordination-forward; the payer seats (levy-bearing householders, dictated-letter women) experience fee-mediated access and compute extraction-forward; merchants sit between, purchasing reach unavailable elsewhere. The sibling readings add a second gap orthogonal to seat position: holders of the liturgical and nativist readings deny the arrangement counts as Hebrew's life at all, so the same two-register corpus reads to them as degradation rather than operation. The engine computes per-seat classifications from the structural data; the cross-reading dismissal is carried in the omega variables, not averaged away here.
 *
 * DIRECTIONALITY LOGIC:
 *   Scribal-rabbinic elites derive near the beneficiary pole: they set norms, collect the fees, and their identity-locked exit deepens rather than offsets that position. Pedagogues are similarly beneficiary-weighted (wages flow in; constrained exit). Communal leadership benefits from the corridor but substitutes court-language channels for state business, keeping it off the extreme beneficiary end. Long-distance merchants are net beneficiaries who also pay — schooling, couriers, scribes — placing them moderately low. Non-literate householders and diaspora women sit near the full-target pole: they fund the channel through levies, reach it only through paid mediation, and face trapped exit. Continental spatial scope raises effective extraction modestly for all seats through verification difficulty; that arithmetic belongs to the engine, not this story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no shared vernacular across the dispersion — stayed live for most of the interval, so the arrangement is not resolved mandatrophy; theater growth at the tail is a drift signal, not yet the defining cost-asymmetry of a maintained shell. Reading the structure as tangled_rope prevents two mislabels: a rope reading would erase the gatekeeping rents (fees, schooling obligations, mediated access) that payer seats demonstrably bear; a snare reading would erase the coordination service that made merchants and communities buy in voluntarily for a millennium. If the founding problem is judged dead at the tail (vernaculars and print having solved intercommunal contact), the mismatch consumer should flag the zombie configuration; authoring founding_problem_status as contested alongside a world_rearranges verdict arms that flag without asserting it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dismissal_contest,
    'Does the bridge-language arrangement instantiate the same continuity commitment that liturgical_preservation and native_generative instantiate, or does it fall outside the kernel as ''not really Hebrew,'' as both siblings maintain?',
    'Compare the three compiled sibling stories: if victim sets, epsilon referents, and persistence mechanisms overlap sufficiently, the kernel holds as one commitment read three ways; if this reading''s referent (the two-register channel arrangement) shares no structural element with the siblings'' referents, the kernel splits into independent commitments.',
    'If the kernel splits, this file detaches from the family network and its classification stands alone; if it holds, the engine reads contamination edges among all three readings and the dismissal becomes a measurable cross-reading effect rather than a framing choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dismissal_contest, conceptual, 'Whether this reading belongs to the same kernel as its dismissing siblings.').

omega_variable(
    two_register_epsilon_decomposition,
    'High-register epistulary production and marketplace pidgin letters serve the same channel but differ in who writes, who pays, and how formulaic the output is — do they carry one epsilon or two?',
    'Register-stratified analysis of the Geniza correspondence: if payer composition and cost incidence differ systematically by register, decompose into two linked stories (high-register channel, pidgin channel) per the epsilon-invariance principle, giving each its own beneficiaries, victims, and metrics.',
    'Decomposition would split this story''s beneficiary/victim structure between the registers and would likely classify the high-register half as more extractive and more theater-laden than the pidgin half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_register_epsilon_decomposition, empirical, 'Guard against forcing one epsilon across structurally distinct registers of the same label.').

omega_variable(
    womens_exclusion_visibility,
    'Is women''s exclusion from the written channel structural (schooling barred) or archival (they used the channel constantly by dictation and orally, but left few self-authored records)?',
    'Quantify the dictated-letter corpus against schooling ordinances by region; if dictated-use rates approach male rates where records survive, the exclusion is substantially archival rather than structural.',
    'If archival, the extraction currently weighted onto the women''s payer seat narrows to the schooling bar itself and the mediator-fees component; if structural, the current victim weighting stands and the trapped-exit attribution strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(womens_exclusion_visibility, empirical, 'Structural versus archival character of the women''s payer seat.').

omega_variable(
    enforcement_vs_incentive_persistence,
    'How much of the channel''s persistence came from incentive (no cheaper cross-communal alternative existed) versus enforcement (communal schooling ordinances, standardized document-language rules, funded courier obligations)?',
    'Compare channel usage across places and periods where vernacular alternatives were legally available against places under explicit communal ordinances; a difference-in-differences across ordinance adoption isolates the enforcement share.',
    'If incentive dominates, the authored suppression overstated coercion and the arrangement sits nearer pure coordination; if enforcement dominates, the tangled_rope reading firms up and the payer seats'' trapped attributions strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_incentive_persistence, empirical, 'Split of persistence between voluntary demand and enforced obligation.').

omega_variable(
    terminal_fragmentation_classification,
    'At the interval''s end the channel thins and correspondence grows ever more formulaic — is late-period Hebrew correspondence a transitional arrangement winding down under a de facto sunset, or a maintained shell drifting toward inertial performance?',
    'Track whether post-1500 Hebrew correspondence volume tracks real coordination demand or ceremonial display, benchmarked against contemporaneous vernacular-channel volumes in the same communities.',
    'A sunset-shaped tail argues the arrangement''s final phase was transitional support being retired; an inertial-drift tail argues the degraded phase belongs to a separate later-stage story with its own epsilon and theater profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_fragmentation_classification, conceptual, 'Persistence question at the interval boundary: wind-down versus shell maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t2, hebrew_continuity__bridge_pidginized, theater_ratio, 2, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t2, observed).
narrative_ontology:measurement(hebr_tr_t4, hebrew_continuity__bridge_pidginized, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t4, observed).
narrative_ontology:measurement(hebr_tr_t6, hebrew_continuity__bridge_pidginized, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t6, observed).
narrative_ontology:measurement(hebr_tr_t8, hebrew_continuity__bridge_pidginized, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__bridge_pidginized, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t10, observed).
narrative_ontology:measurement(hebr_tr_t13, hebrew_continuity__bridge_pidginized, theater_ratio, 13, 0.3).
narrative_ontology:measurement_basis(hebr_tr_t13, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t2, hebrew_continuity__bridge_pidginized, base_extractiveness, 2, 0.3).
narrative_ontology:measurement_basis(hebr_be_t2, observed).
narrative_ontology:measurement(hebr_be_t4, hebrew_continuity__bridge_pidginized, base_extractiveness, 4, 0.34).
narrative_ontology:measurement_basis(hebr_be_t4, observed).
narrative_ontology:measurement(hebr_be_t6, hebrew_continuity__bridge_pidginized, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(hebr_be_t6, observed).
narrative_ontology:measurement(hebr_be_t8, hebrew_continuity__bridge_pidginized, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__bridge_pidginized, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(hebr_be_t10, observed).
narrative_ontology:measurement(hebr_be_t13, hebrew_continuity__bridge_pidginized, base_extractiveness, 13, 0.38).
narrative_ontology:measurement_basis(hebr_be_t13, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.16).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t2, hebrew_continuity__bridge_pidginized, suppression_requirement, 2, 0.21).
narrative_ontology:measurement_basis(hebr_su_t2, observed).
narrative_ontology:measurement(hebr_su_t4, hebrew_continuity__bridge_pidginized, suppression_requirement, 4, 0.27).
narrative_ontology:measurement_basis(hebr_su_t4, observed).
narrative_ontology:measurement(hebr_su_t6, hebrew_continuity__bridge_pidginized, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(hebr_su_t6, observed).
narrative_ontology:measurement(hebr_su_t8, hebrew_continuity__bridge_pidginized, suppression_requirement, 8, 0.36).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__bridge_pidginized, suppression_requirement, 10, 0.31).
narrative_ontology:measurement_basis(hebr_su_t10, observed).
narrative_ontology:measurement(hebr_su_t13, hebrew_continuity__bridge_pidginized, suppression_requirement, 13, 0.26).
narrative_ontology:measurement_basis(hebr_su_t13, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.07).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% Family decomposition of the colloquial label 'Hebrew continuity' into three epsilon-invariant constraint stories: hebrew_continuity__liturgical_preservation authors epsilon for the recitation-and-transmission maintenance apparatus; hebrew_continuity__native_generative authors epsilon for the native-acquisition requirement; this file authors epsilon for the contact-language channel arrangement (sparse natives, two-register usage, instrumental occupancy of the kernel). Each story has its own beneficiary/victim structure and its own stable epsilon. Upstream/downstream ordering runs liturgical_preservation -> this reading -> native_generative: the transmitted canon supplied the textual material the channel deployed, and the channel's documented millennium of operation is the datum the nativist reading defines itself against. All three files declare these links in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
