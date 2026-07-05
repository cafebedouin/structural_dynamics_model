% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection: Brandenburg Imminent-Lawless-Action Standard
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the speech-protection
 *   kernel: the Brandenburg v. Ohio (1969) standard, under which the
 *   government may not proscribe advocacy of force or law violation except
 *   where such advocacy is directed to inciting or producing imminent lawless
 *   action and is likely to produce such action. The standard replaced the
 *   earlier 'clear and present danger' and 'bad tendency' tests, dramatically
 *   narrowing the unprotected set. Since 1969 it has proven extremely stable
 *   at the doctrinal core, applied by courts to protect Klan rally rhetoric,
 *   violent-themed political advocacy, and a wide range of offensive
 *   expression. The rising extractiveness series reflects not doctrinal
 *   change (the legal test itself is essentially frozen) but the growing
 *   recognition, documented in scholarship and civil-rights litigation over
 *   five decades, that the standard's harm-externalization onto targeted
 *   communities has become more visible and more contested as digital
 *   platforms amplify sustained, coordinated, non-imminent harassment that
 *   the doctrine was never designed to address. This is a single, ε-stable
 *   reading — the sibling readings (harm_limited_reading, balancing_reading)
 *   are separate constraint stories with their own ε values, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - federal_judiciary: sets and enforces the imminence line (institutional/analytical)
 *   - political_dissidents and controversial_speakers: primary beneficiaries of the wide protected zone (moderate-organized/mobile)
 *   - civil_liberties_organizations: active agenda-setting beneficiary that litigates to preserve the line (organized/mobile)
 *   - targeted_minority_communities and harassment_targets: bear the externalized harm the doctrine does not reach (powerless/trapped-constrained)
 *   - state_and_local_legislatures: excluded from responsive harm-balancing (institutional/constrained)
 *   - constitutional_scholars: analytical observers of the doctrine's history and comparative standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.38).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.22).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection: Brandenburg Imminent-Lawless-Action Standard").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, '0af730c9-2d9b-4d03-b2fc-3f6da8698daa').
narrative_ontology:cs_kernel_codification('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', formalized).
narrative_ontology:cs_authority_grounding('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', lineage).
narrative_ontology:cs_interpretation_layer_present('0af730c9-2d9b-4d03-b2fc-3f6da8698daa').
narrative_ontology:cs_reading_relation('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', speech_protection_boundary__harm_limited_reading, forecloses).
narrative_ontology:cs_reading_relation('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', foundational, anti_suppression_priority_over_dignitary_harm).
narrative_ontology:cs_axiom_status(anti_suppression_priority_over_dignitary_harm, holdable).
narrative_ontology:cs_axiom_grounding('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', anti_suppression_priority_over_dignitary_harm, deontological).
narrative_ontology:cs_axiom('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', secondary, bright_line_imminence_test_superior_to_case_by_case_weighing).
narrative_ontology:cs_axiom_status(bright_line_imminence_test_superior_to_case_by_case_weighing, holdable).
narrative_ontology:cs_axiom_grounding('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', bright_line_imminence_test_superior_to_case_by_case_weighing, instrumental).
narrative_ontology:cs_reference_frame('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', post_brandenburg_imminence_settlement).
narrative_ontology:cs_drift_state('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', digital_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0af730c9-2d9b-4d03-b2fc-3f6da8698daa', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, press_institutions).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targeted_minority_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, harassment_targets).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, counter_speech_remedy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies and enforces the Brandenburg imminence test in every First Amendment challenge, striking down restrictions that fall short of direct incitement to imminent lawless action. Sets the operative boundary of the protected/unprotected set and has sole authority to revise it through subsequent case law.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Rely on the near-absolute standard to voice unpopular, anti-government, or radical political positions without fear of prosecution short of direct calls to imminent violence. The high bar for restriction is precisely what protects organizing, protest rhetoric, and dissenting speech from suppression by hostile majorities or state actors.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, political_dissidents, beneficiary,
    moderate, biographical, mobile, national).

% Includes provocateurs, extremist organizers, and fringe ideological groups who use the wide protected zone to disseminate inflammatory rhetoric that stops short of direct incitement. They benefit from a bright-line rule that shields nearly all content-based speech from liability regardless of downstream social cost.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, controversial_speakers, beneficiary,
    organized, biographical, mobile, national).

% Depend on the wide protected zone to publish investigative material, opinion, and criticism of powerful actors without a case-by-case harm balancing test that could chill reporting. The imminence standard gives clear, litigable boundaries that make editorial risk predictable.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, press_institutions, beneficiary,
    powerful, generational, mobile, national).

% Litigate to preserve and extend the Brandenburg line, treating it as the load-bearing doctrine protecting all future speech from erosion. They actively shape enforcement by bringing test cases and filing amicus briefs whenever lower courts drift toward balancing tests.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, civil_liberties_organizations, agenda_setter).

% Absorb the aggregate social cost of protected hate speech, dehumanizing rhetoric, and organized harassment campaigns that never cross the imminence line. Cannot obtain injunctive or civil relief against the speech itself; their recourse is confined to counter-speech or after-the-fact remedies for conduct that has already escalated past pure speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targeted_minority_communities, payer,
    powerless, generational, trapped, national).

% Individuals subjected to sustained, coordinated but non-imminent threatening or degrading speech (doxxing campaigns, sustained online harassment mobs) find that the standard treats each individual utterance as protected even where the cumulative pattern is severe. Exit means withdrawing from public platforms or professional visibility altogether.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harassment_targets, payer,
    powerless, biographical, constrained, local).

% Attempt to pass hate-speech, harassment, or dignity-protective statutes responsive to constituent harm, but see them struck down or chilled in drafting by the imminence standard. Their considered harm-balancing judgments are foreclosed before they can be tested against local conditions.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, state_and_local_legislatures, excluded,
    institutional, biographical, constrained, regional).

% Study the doctrine's history, its departure from earlier balancing-era precedent, and its comparative divergence from peer democracies. Document both the anti-suppression benefits and the externalized harms without holding an enforcement role.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, content-neutral rule that lets speakers, publishers, and lower courts know in advance what speech is protected, avoiding the chilling effect and inconsistency of ad hoc harm-balancing across thousands of local actors.
% TRANSFER_FUNCTION: Moves the burden of unrest, dignitary harm, and psychological/social cost from would-be censors and majoritarian institutions onto communities and individuals targeted by protected-but-harmful speech, in exchange for insulating all speakers (including dissidents) from majoritarian suppression.
% ABSENT_VOICES: Targeted minority communities and sustained-harassment victims are rarely direct parties to the landmark cases that set and re-affirm the standard; the doctrine is overwhelmingly litigated by speakers, publishers, and civil liberties organizations defending the protected zone, not by those bearing its externalities.
% DISAPPEARANCE_RATIONALE: If the near-absolute standard were replaced overnight by a harm-balancing or harm-limited regime, political organizing rhetoric, provocative journalism, and fringe advocacy would face new prosecutorial and civil exposure; legislatures would immediately begin passing content-based restrictions previously foreclosed; civil liberties litigation dockets would shift from defending the line to contesting its erosion case by case.
% FOUNDING_PROBLEM: Built to end the World War I/Red Scare-era regime (Schenck's 'clear and present danger', Whitney) under which political dissidents, labor organizers, and radicals were imprisoned for speech far short of any actual violence, on the theory that mere tendency toward bad outcomes justified suppression.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties organizations and much of the legal academy attest the original problem — state suppression of dissident political speech — remains live and the standard is still doing that protective work. Critical race theorists, comparative constitutional scholars, and representatives of targeted communities attest from outside the beneficiary set that the standard now also functions to shield organized harassment and hate speech that has no meaningful nexus to the anti-dissident-suppression problem it was built to solve, and that peer democracies achieve dissident protection without the same externalized cost.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).
:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at 2025) rather than high because the doctrine's core function — protecting political speech from majoritarian suppression — is a genuine and substantial coordination good, not a pretext; the extraction here is the diffuse externality borne by targeted communities, not rent capture by an identifiable extractive institution. Suppression is comparatively low (0.22) because the standard's entire operative logic is to MINIMIZE suppression of speech; what suppression exists is directed at the excluded legislative alternative (state/local harm-balancing statutes struck down under this doctrine) rather than at speakers. Theater ratio is low and rising slowly (0.08 to 0.15) reflecting that enforcement is substantively real (courts genuinely apply the imminence test) with only a small and growing performative component (invocations of free-speech absolutism in contexts where imminence analysis is not seriously contested). Accessibility collapse is moderately high (0.62): once a court holds speech falls short of Brandenburg imminence, essentially no legal remedy exists for the harmed party against the speech itself — that is a real, not partial, collapse of legal alternatives for the payer seats, even though political and organizing alternatives (counter-speech, platform-level moderation, social sanction) remain nominally available.
 *
 * PERSPECTIVAL GAP:
 *   From the federal judiciary and civil liberties organization seats, the standard is nearly self-evidently correct: it is the settled bulwark against a well-documented historical harm (political persecution via speech suppression), defended by clear precedent and institutional consensus. From the targeted minority community and harassment target seats, the same rule computes as a structure that treats their sustained, well-documented harm as categorically outside the law's concern so long as no single utterance meets the imminence threshold. The engine's per-seat computation should reflect this: agenda-setter and organized beneficiary seats trend toward rope/coordination readings; powerless, trapped payer seats trend toward tangled_rope or snare-adjacent readings of the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Political dissidents, controversial speakers, press institutions, and civil liberties organizations sit near the beneficiary end of directionality: the doctrine subsidizes their speech activity by removing legal risk that a harm-balancing regime would reintroduce. Targeted minority communities and harassment targets sit near the target end: they bear a cost (dignitary, psychological, social) generated by protected speech, with no doctrinal recourse against the speech itself, and their exit options are trapped or constrained (withdrawal from public life, geographic relocation, or acceptance of continued exposure). State and local legislatures are excluded rather than positioned along the beneficiary-victim axis proper — their harm-balancing judgments are foreclosed procedurally, which is a different structural relationship than bearing extraction directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state suppression of political dissidents via loosely-defined 'bad tendency' liability) is genuinely still live in the sense that authoritarian-leaning prosecutions of protest speech remain a live risk in some jurisdictions and eras — this argues against treating the doctrine as a pure zombie mandate. But the founding-problem status is authored as contested rather than flatly live, because the doctrine's current primary friction point (sustained non-imminent harassment and hate speech at internet scale) is a problem the 1969 framers were not solving and could not have anticipated; the standard persists unchanged while the harm profile it interacts with has shifted substantially. This is precisely the kind of divergence the tangled_rope classification is built to hold: a genuine, still-active coordination function (protecting dissident speech from majoritarian suppression) coexisting with asymmetric extraction (externalized harm concentrated on communities who were not parties to the doctrine's founding cases) under active judicial enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_line_naturalness,
    'Is the imminence threshold a principled, non-arbitrary line derived from free-speech theory, or a contingent historical settlement reflecting the specific anxieties of the Cold War/civil-rights era court that could have been drawn elsewhere?',
    'Comparative doctrinal history: examine whether courts applying similar free-expression commitments (Canada, Germany, ECHR jurisprudence) converge on a similar imminence-style line or diverge toward dignity-based or harm-based tests, and whether the U.S. line has shifted at the margins since 1969.',
    'If the line is arbitrary/contingent rather than principled, the absolutist reading''s claim to be the uniquely correct instantiation of free-speech commitments weakens, and the externalized harm looks more like a policy choice than a structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_line_naturalness, conceptual, 'Whether the Brandenburg imminence threshold is principled or historically contingent.').

omega_variable(
    aggregate_harm_measurability,
    'Can the aggregate, diffuse harm borne by targeted minority communities and harassment targets under this doctrine be measured with enough rigor to weigh against the doctrine''s anti-suppression benefit, or is it structurally resistant to the kind of quantification the legal system uses to justify restrictions?',
    'Longitudinal social-science study correlating protected hate-speech/harassment incidence with documented psychological, economic, and civic-participation harms in affected communities, compared across jurisdictions with different speech-protection regimes.',
    'If harm is robustly measurable and substantial, the tangled_rope classification''s victim declaration is strongly supported empirically; if harm resists measurement, the classification rests more on normative judgment about which costs count.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_harm_measurability, empirical, 'Whether the externalized harm this reading tolerates is empirically measurable at the scale needed to weigh it against protection benefits.').

omega_variable(
    reading_selection_is_contested_political_fact,
    'Is the choice among the three kernel readings (absolutist, harm_limited, balancing) itself a matter that could be resolved by legal reasoning, or is it an irreducibly contested political/moral choice about which value (anti-suppression vs. anti-subordination) takes priority when they conflict?',
    'This is likely not resolvable by further legal analysis alone; track whether comparative constitutional convergence occurs over multi-decade horizons, or whether the disagreement persists as a stable feature of pluralist democracies with genuinely different political traditions.',
    'If irreducibly contested, no single reading can claim to be the ''true'' resolution of the kernel, and each reading''s classification should be understood as a description of one coherent, defensible legal tradition rather than a diagnosis of error in the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_is_contested_political_fact, preference, 'Whether the kernel''s reading-selection is a resolvable legal question or an irreducible value conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.08).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.09).
narrative_ontology:measurement(spee_tr_t1995, speech_protection_boundary__absolutist_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_boundary__absolutist_reading, theater_ratio, 2005, 0.11).
narrative_ontology:measurement(spee_tr_t2015, speech_protection_boundary__absolutist_reading, theater_ratio, 2015, 0.13).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_boundary__absolutist_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.18).
narrative_ontology:measurement(spee_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(spee_be_t1995, speech_protection_boundary__absolutist_reading, base_extractiveness, 1995, 0.24).
narrative_ontology:measurement(spee_be_t2005, speech_protection_boundary__absolutist_reading, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(spee_be_t2015, speech_protection_boundary__absolutist_reading, base_extractiveness, 2015, 0.33).
narrative_ontology:measurement(spee_be_t2025, speech_protection_boundary__absolutist_reading, base_extractiveness, 2025, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_boundary__absolutist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the speech_protection_boundary kernel. absolutist_reading maximizes the protected set and narrows the unprotected set to direct incitement of imminent lawless action, producing lower measured suppression and moderate extractiveness concentrated as an externality on targeted communities. harm_limited_reading and balancing_reading are separate constraint stories with their own beneficiary/victim sets and independently authored ε values (expected to differ substantially, reflecting genuinely different coordination/extraction profiles, not measurement noise on one constraint). Network edges here mark kernel co-membership and downstream doctrinal pressure: adoption or erosion of the absolutist line directly changes the legitimacy conditions and resource availability for the sibling readings' litigation strategies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
