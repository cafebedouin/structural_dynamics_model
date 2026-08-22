% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Popular Sovereignty Reading of Constitutional Authority
 *   domain: political/legal/philosophical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_text kernel:
 *   the popular-sovereignty reading, under which the text's authority derives
 *   from the constituent power of the demos and neither courts nor
 *   legislature hold final interpretive authority — the people retain it
 *   through amendment, convention, or revolution. Per the epsilon-invariance
 *   principle, the sibling readings (judicial_supremacy_reading,
 *   legislative_sovereignty_reading) are separate constraint stories with
 *   their own epsilon values, beneficiary structures, and classifications;
 *   this file authors only the popular-sovereignty instantiation and does not
 *   hedge across readings. The epsilon referent is the standing arrangement
 *   under contest — the popular-sovereignty allocation itself — assessed by
 *   the reading's own lights: a popular-sovereignty theorist sees the
 *   arrangement as the legitimate baseline of self-government while
 *   acknowledging real costs imposed on institutional seats and
 *   stability-dependent groups. KEY AGENTS (by structural relationship): -
 *   popular_movements: Primary beneficiary and enforcement arm
 *   (organized/constrained) — collects meta-authority, administers it when
 *   exercised - electoral_majorities: Episodic beneficiary
 *   (organized/constrained) - constitutional_courts: Primary target
 *   (institutional/trapped) — bears loss of finality - professional_jurists:
 *   Secondary target (organized/constrained) — expertise subordinated -
 *   institutional_minorities: Sharpest-cost target (powerless/trapped) -
 *   legislatures: Dual-positioned payer/beneficiary (institutional/trapped) -
 *   comparative_constitutionalists: Analytical observer — sees full structure
 *
 * KEY AGENTS:
 *   - popular_movements: Primary beneficiary and enforcement arm (organized/constrained) — collects the meta-authority grant and administers the arrangement when exercised
 *   - electoral_majorities: Episodic beneficiary (organized/constrained) — binds institutions when mobilized, ordinary subjects otherwise
 *   - constitutional_courts: Primary target (institutional/trapped) — every judgment provisional, exposed to curbing and override
 *   - professional_jurists: Secondary target (organized/constrained) — doctrinal expertise subordinated to popular acceptance
 *   - institutional_minorities: Sharpest-cost target (powerless/trapped) — depend on the institutional stability the arrangement renders provisional
 *   - legislatures: Dual-positioned payer/beneficiary (institutional/trapped) — gains override power, loses enactment finality
 *   - comparative_constitutionalists: Analytical observer (analytical/analytical) — documents the structure across systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.43).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.64).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.43).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Popular Sovereignty Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "political/legal/philosophical").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'ab58e5ed-fe2f-4f14-9a23-65edc7a050e0').
narrative_ontology:cs_kernel_codification('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', fixed_text).
narrative_ontology:cs_authority_grounding('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', lineage).
narrative_ontology:cs_reading_relation('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', foundational, ultimate_authority_resides_in_demos).
narrative_ontology:cs_axiom_status(ultimate_authority_resides_in_demos, holdable).
narrative_ontology:cs_axiom_grounding('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', ultimate_authority_resides_in_demos, deontological).
narrative_ontology:cs_axiom('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', secondary, institutional_interpretation_is_provisional).
narrative_ontology:cs_axiom_status(institutional_interpretation_is_provisional, holdable).
narrative_ontology:cs_axiom_grounding('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', institutional_interpretation_is_provisional, deontological).
narrative_ontology:cs_reference_frame('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', constituent_power_supremacy_framework).
narrative_ontology:cs_drift_state('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', contemporary_populist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ab58e5ed-fe2f-4f14-9a23-65edc7a050e0', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, electoral_majorities).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, professional_jurists).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_minorities).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislatures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, legislatures).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized citizen mobilizations — constituent-assembly drives, amendment campaigns, mass movements that invoke the authority of the people against existing institutional settlements. The arrangement grants them standing to override both courts and legislatures; when they succeed they also run the process, convening conventions and drafting amendments. They cannot exit the polity they act upon; their leverage exists only inside it.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, popular_movements, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, popular_movements, agenda_setter).

% Voting majorities, episodically mobilized. When aroused, their choices bind every institution without judicial veto or supermajority buffer. Between mobilizations they live under whatever settlement currently prevails and hold no special standing beyond the ordinary franchise.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, electoral_majorities, beneficiary,
    organized, biographical, constrained, national).

% Courts decide cases with apparent finality day to day, but under this arrangement every judgment is provisional — open to reversal by amendment, by convention, or by mobilized refusal to comply. Recurring court-curbing proposals, jurisdiction-stripping bills, and public delegitimation campaigns are the standing price of the office. A court cannot move its jurisdiction elsewhere.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_courts, payer,
    institutional, generational, trapped, national).

% Judges, law faculties, and bar associations whose standing rests on doctrinal expertise. The arrangement subordinates expertise to popular will: their interpretations persuade only insofar as the public accepts them. Retreat into academe or foreign jurisdictions preserves income but forfeits the practice they trained for.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, professional_jurists, payer,
    organized, biographical, constrained, global).

% Groups whose safety has historically depended on insulated courts, entrenched rights, and professionalized administration. Moments of unrestrained popular will expose them directly: they are outvoted in conventions, swept by majoritarian fervor, and cannot leave the jurisdiction that governs them.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_minorities, payer,
    powerless, generational, trapped, national).

% Parliaments and congresses. They gain the power to override judicial rulings, but the same arrangement strips their own enactments of finality — every statute stands equally provisional before the people. They absorb the standing cost of referendum threats, recall campaigns, and convention calls, while occasionally collecting the benefit of overturning courts.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislatures, payer,
    institutional, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, legislatures, beneficiary).

% Scholars who compare how different systems allocate final constitutional authority. They document the recurrence of supremacy claims, the varying vitality of amendment channels, and the life cycles of constituent moments. They neither run the arrangement nor bear its costs.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutionalists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, popular_movements).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors the legitimacy chain of constitutional order in the governed: by denying any institution final interpretive authority, it addresses the problem of institutional self-capture — no court or parliament can entrench its own reading as unappealable — and preserves standing channels (amendment, convention, revolution) through which fundamental disagreement is resolved.
% TRANSFER_FUNCTION: Moves interpretive authority and decision-finality from courts and legislatures to mobilized popular actors; correspondingly moves the costs of unsettled meaning — instability, uncertainty, exposure of protected positions — onto institutional officeholders and onto those who depend on institutional stability for their protection.
% ABSENT_VOICES: Those outvoted or overrun when 'the people' speak: minorities within the mobilized majority, future generations bound by a convention's output, and legal experts whose objections are discounted as self-interested defense of privilege. They are absent precisely at the moment of exercise — present only as voters afterward or litigants before the next mobilization.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, courts and legislatures would immediately contest the vacated finality — the exact contest the two sibling readings stage — and constitutional politics would reorganize around whichever branch seized it. Amendment channels would lose their warrant, mobilized publics would lose standing to override institutional settlements, and the burden of constitutional change would shift entirely to ordinary legislation and litigation.
% FOUNDING_PROBLEM: Grounding constitutional authority in the governed rather than in monarch, tradition, or divine right — and doing so permanently, so that no successor institution, elected or appointed, could congeal into a new sovereign.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative constitutional scholarship documents recurring institutional supremacy claims across systems (court-curbing episodes, override-clause fights, parliamentary-primacy disputes); the historical record of inter-branch conflict attests the problem recurs; and the existence of two rival readings each claiming to solve the same capture problem confirms the problem is live even among parties who reject this reading's remedy.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.43, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are independent authored facts. CLAIMED TYPE tangled_rope: the arrangement possesses a genuine coordination function (it maintains the legitimacy chain and prevents institutional self-capture — a real collective-action problem) AND asymmetric extraction through the same structure (institutional seats and stability-dependent groups pay standing costs that mobilized actors do not), held together by active enforcement (the credible possibility of amendment, convention, or mobilized refusal). METRICS, authored descriptively: extractiveness 0.43 — real but bounded; the transfer is authority and stability rather than money, and the reading's own lights discount part of the cost as the legitimate price of self-government. Suppression 0.64 — the arrangement persists by actively delegitimating rival claims to finality (court-curbing rhetoric, convention threats, expertise-bashing); suppression is structural (mobilized counter-pressure) more than violent, and is authored unscaled per the raw-property rule. Theater_ratio 0.52 — a growing share of 'the people' invocations now serve ordinary partisan projects rather than genuine constituent moments, and amendment channels sit frozen in most systems; the series crosses 0.5 at the endpoint, a Goodhart-drift signal the engine should weigh. Accessibility_collapse 0.45 — the rival readings persist robustly; understanding this reading does not collapse judicial-supremacy or parliamentary-supremacy practice. Resistance 0.62 — courts and the legal profession defend their authority continuously and effectively. The measurement series run on ONE shared time grid (1945–2025, seven points); every tracked metric is authored at every point, so no scalar substitution contaminates earlier rows. The interval maps to the modern constitutional era: the postwar constituent-assembly wave at t=0, the global expansion of judicial review through the middle points, and the populist-revival period at the endpoint.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp and structural. From the popular_movements and electoral_majorities seats the arrangement computes as near-pure coordination: it grants standing, solves the capture problem, and its costs fall elsewhere. From the constitutional_courts and institutional_minorities seats the same structure computes as heavily extractive: courts surrender the finality that constitutes their function, and minorities absorb the tail risk of every unrestrained mobilization. The same-level comparison between courts and legislatures (both institutional power, both trapped exit) shows how constraint-specific factors differentiate experience despite equal nominal standing: legislatures are partially compensated (override power over courts) while courts are uncompensated (their core claim IS finality), so the payer seat splits internally. Professional_jurists occupy a middle register — organized enough to shape discourse, constrained enough that discourse-shaping no longer decides outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation without needing overrides. popular_movements and electoral_majorities sit near the beneficiary end (low d): the arrangement subsidizes them with standing they would otherwise lack, and their constrained exit reflects residence in the polity rather than exposure to the arrangement. constitutional_courts, professional_jurists, and institutional_minorities sit near the target end (high d): courts and jurists pay in authority and standing, minorities pay in exposure, and trapped or near-trapped exit pushes them toward the full-target pole — institutional_minorities (powerless, trapped) derive nearest d=1.0, which matches their descriptive position as the seat bearing the sharpest uncompensated costs. legislatures derive mid-to-high: the dual beneficiary/payer declaration nets out below courts because override power partially offsets lost finality. No directionality_overrides are authored: the derivation from roles, power atoms, and exit options already produces the correct ordering, and the override mechanism keys on power atoms, which cannot distinguish the two institutional payers anyway — their difference is carried by the role declarations instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — grounding authority in the governed so no institution congeals into a new sovereign — remains live: institutional supremacy claims recur in every documented system, and the two sibling readings exist precisely because the problem admits rival solutions. The R5 mismatch consumer therefore reads founding_problem_status=live against disappearance_verdict=world_rearranges: aligned, no zombie flag. The classification prevents mislabeling in both directions: calling this a snare would erase the genuine coordination function (legitimacy-chain maintenance) that even hostile seats implicitly rely on; calling it a rope would erase the asymmetric, enforced costs borne by courts, jurists, and minorities. It is not a scaffold — no sunset clause; the reading intends permanence. It is not a piton — the function still fires when mobilized (constituent assemblies, citizens' assemblies, amendment waves), though the theater series crossing 0.5 warns that an increasing fraction of operation is performative invocation without exercise. Mandatrophy is NOT resolved; the arrangement's mandate tracks a persistent problem, but its exercise channels are decaying relative to its rhetorical deployment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_seat_of_finality_contest,
    'This constraint is one reading of the constitutional_text kernel (popular_sovereignty_reading); the sibling readings judicial_supremacy_reading and legislative_sovereignty_reading allocate the same finality to courts or parliament respectively. Where, structurally, does final interpretive authority reside?',
    'No intra-framework resolution exists — the readings'' core premises directly contradict on the seat of finality. Resolution arrives only through constitutional crisis or design choice: a jurisdiction adopting one reading as operative law resolves it locally; comparative analysis can only map which jurisdictions hold which reading.',
    'If a sibling reading were adopted as the operative constraint, this story''s beneficiary/victim structure inverts: courts (or legislatures) become the primary beneficiaries and mobilized publics lose standing; effective extraction redistributes accordingly. Cross-reading epsilon comparisons are meaningless — they measure different constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_seat_of_finality_contest, conceptual, 'Committer-frame omega: kernel membership, sibling readings, and the located disagreement (seat of final interpretive authority).').

omega_variable(
    demos_identification_ambiguity,
    'Who counts as ''the people'' whose authority is ultimate — the majoritarian electorate, the deliberative public sphere, or the mobilized plurality that physically occupies the streets and conventions?',
    'Conceptual analysis of the reading''s own tradition (Lockean, Sieyesian, Arendtian strands identify the demos differently), combined with observing which identification actual exercises channels reward.',
    'A majoritarian identification concentrates beneficiary status in electoral_majorities and maximizes minority exposure; a deliberative identification diffuses benefits toward civic_participation generally and reduces the tail risk borne by institutional_minorities. The beneficiary structure and the minority-seat extraction both shift with the identification chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_identification_ambiguity, conceptual, 'The reading''s beneficiary structure depends on an unresolved identification of the sovereign body.').

omega_variable(
    exercise_channel_weighting,
    'Which channels legitimately express constituent power — formal amendment, ad hoc convention, or revolution — and in what proportion does each carry the reading''s force?',
    'Track which channels actual constituent moments use and which the reading''s adherents recognize as binding; doctrinal history of constituent-power theory supplies the weighting the tradition itself endorses.',
    'If only formal amendment counts, the reading converges in practice toward legislative sovereignty (amendment runs through legislatures) and its distinctness erodes; if revolution counts as a live channel, the exposure costs borne by institutional_minorities dominate and the arrangement''s suppressive face sharpens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exercise_channel_weighting, conceptual, 'The reading''s practical content varies with the relative legitimacy assigned to its three exercise channels.').

omega_variable(
    amendment_vitality_decay,
    'Does the long decline in constitutional amendment frequency across mature democracies indicate the reading''s exercise channels have become dead letter (failure), or that constitutional settlement is so widely accepted that exercise is simply unnecessary (success)?',
    'Comparative amendment-rate data correlated with survey measures of constitutional satisfaction and with the incidence of attempted-but-blocked amendments; a high blocked-attempt rate amid low success indicates channel decay rather than settlement.',
    'If channels are dead letter, the theater_ratio series understates functional decay and the arrangement drifts toward inertial persistence in practice — maintained by invocation rather than exercise — even while its claimed structure remains intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_vitality_decay, empirical, 'Whether falling exercise rates reflect channel decay or successful settlement.').

omega_variable(
    minority_exposure_legitimacy,
    'Are the stability and expertise costs borne by institutional_minorities a legitimate price of popular self-government, or an extraction severe enough to invalidate the arrangement''s coordination claim from that seat?',
    'Not resolvable by data alone — it turns on prior commitments about the moral weight of majoritarian self-determination versus counter-majoritarian protection. Constitutional-design outcomes (rights entrenchment, supermajority amendment thresholds) reveal how societies have priced the tradeoff.',
    'If the costs are judged illegitimate, the minority seat''s computed classification trends toward pure extraction and the coordination-function claim weakens corpus-wide; if legitimate, the arrangement''s extractiveness is bounded by design choices that insulate minorities without abolishing popular finality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_exposure_legitimacy, preference, 'Value-dependent assessment of whether the minority seat''s costs disqualify the coordination claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_text_psr_tr_t1945, constitutional_text__popular_sovereignty_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t1945, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t1960, constitutional_text__popular_sovereignty_reading, theater_ratio, 1960, 0.26).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t1960, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t1975, constitutional_text__popular_sovereignty_reading, theater_ratio, 1975, 0.31).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t1975, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t1990, constitutional_text__popular_sovereignty_reading, theater_ratio, 1990, 0.36).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t1990, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t2005, constitutional_text__popular_sovereignty_reading, theater_ratio, 2005, 0.41).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t2005, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t2015, constitutional_text__popular_sovereignty_reading, theater_ratio, 2015, 0.47).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t2015, observed).
narrative_ontology:measurement(constitutional_text_psr_tr_t2025, constitutional_text__popular_sovereignty_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(constitutional_text_psr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(constitutional_text_psr_be_t1945, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1945, 0.28).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t1945, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t1960, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1960, 0.31).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t1960, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t1975, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1975, 0.33).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t1975, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t1990, constitutional_text__popular_sovereignty_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t1990, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t2005, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t2005, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t2015, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t2015, observed).
narrative_ontology:measurement(constitutional_text_psr_be_t2025, constitutional_text__popular_sovereignty_reading, base_extractiveness, 2025, 0.43).
narrative_ontology:measurement_basis(constitutional_text_psr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_text_psr_su_t1945, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t1945, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t1960, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1960, 0.51).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t1960, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t1975, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1975, 0.53).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t1975, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t1990, constitutional_text__popular_sovereignty_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t1990, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t2005, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t2005, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t2015, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t2015, observed).
narrative_ontology:measurement(constitutional_text_psr_su_t2025, constitutional_text__popular_sovereignty_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(constitutional_text_psr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional authority' decomposes into three structurally distinct readings of the constitutional_text kernel, differing on exactly one element — the seat of final interpretive authority. Each member carries its own epsilon, beneficiary/victim structure, and classification; they are linked here per the epsilon-invariance principle. The popular-sovereignty reading exerts structural pressure on both siblings: every assertion of extra-institutional democratic authority raises the legitimacy cost of judicial or parliamentary finality claims, without logically resolving which reading a given jurisdiction operates under. Sibling files should reciprocate the link and document their own deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
