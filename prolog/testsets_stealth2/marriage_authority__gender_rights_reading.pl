% ============================================================================
% CONSTRAINT STORY: marriage_authority__gender_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__gender_rights_reading, []).

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
 *   constraint_id: marriage_authority__gender_rights_reading
 *   human_readable: Intra-Community Marriage Authority under the Gender-Rights Reading
 *   domain: legal pluralism / constitutional law / comparative family law
 *
 * SUMMARY:
 *   Within religious communities that maintain their own family law, a
 *   specific bundle of practices governs how marriages end and what follows:
 *   dissolution at one party's will, maintenance that expires after a short
 *   prescribed period, and property division that steps women aside for male
 *   heirs. The gender-rights reading contests this bundle on grounds of
 *   intra-community gender equality and pursues reform through judicial
 *   expansion of constitutional equality guarantees — case-by-case
 *   nullification of specific practices rather than wholesale codification.
 *   This story authors that standing arrangement as the gender-rights reading
 *   sees it: the epsilon referent is the existing practice bundle, never the
 *   equality-floor regime the reading would put in place. Manifest refinement
 *   note: the seed hypothesis named women_rights_advocates as the beneficiary
 *   and the community women as victims; analysis relocates the primary
 *   collection to male lineage households and communal authorities (who
 *   receive the material and jurisdictional flows), while retaining advocates
 *   as a genuine but secondary beneficiary seat — their institutional
 *   existence is fed by the contest's persistence, not by the arrangement's
 *   operation. The claim and the metrics are independent authored facts:
 *   claimed_type states this reading's structural assessment; the metrics
 *   describe observed operation over 1985-2025 (interval t=0 to t=40).
 *
 * KEY AGENTS:
 *   - communal_religious_authorities: agenda-setting administrator (organized/identity_locked) — runs the councils and offices that solemnize, dissolve, and settle; jurisdiction depends on the arrangement continuing
 *   - male_lineage_households: primary collector (powerful/arbitrage) — receives obligation-shedding and property preference at dissolution events; shifts form when formal rules tighten
 *   - women_within_patriarchal_personal_law: primary bearer (powerless/trapped) — lives the practices; exit costs span income, children, and community simultaneously
 *   - women_rights_advocates: dual-positioned challenger-collector (organized/identity_locked) — runs the constitutional contest; mandate depends on the contest staying open
 *   - constitutional_judiciary: agenda-setting reviser (institutional/constrained) — converts challenges into practice-level rulings and precedent
 *   - ordinary_minority_community_members: collateral bearers (moderate/constrained) — carry stigma and loyalty costs of each public round
 *   - national_legislature: absent seat (institutional/mobile) — holds replacement power, declined to use it since the 1980s rollback
 *   - international_treaty_bodies: analytical observer (institutional/analytical) — periodic reputational pressure from outside the national contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, 0.74).
domain_priors:suppression_score(marriage_authority__gender_rights_reading, 0.82).
domain_priors:theater_ratio(marriage_authority__gender_rights_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority__gender_rights_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__gender_rights_reading, snare).
narrative_ontology:human_readable(marriage_authority__gender_rights_reading, "Intra-Community Marriage Authority under the Gender-Rights Reading").
narrative_ontology:topic_domain(marriage_authority__gender_rights_reading, "legal pluralism / constitutional law / comparative family law").

domain_priors:requires_active_enforcement(marriage_authority__gender_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__gender_rights_reading, 'c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5').
narrative_ontology:cs_kernel_codification('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', distributed).
narrative_ontology:cs_authority_grounding('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', lineage).
narrative_ontology:cs_interpretation_layer_present('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5').
narrative_ontology:cs_reading_relation('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', marriage_authority__secularist_reading, influences).
narrative_ontology:cs_reading_relation('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', foundational, constitutional_equality_supremacy_over_personal_law).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', constitutional_equality_supremacy_over_personal_law, deontological).
narrative_ontology:cs_axiom('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', secondary, practice_targeted_nullification_over_system_replacement).
narrative_ontology:cs_axiom_status(practice_targeted_nullification_over_system_replacement, holdable).
narrative_ontology:cs_axiom_grounding('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', practice_targeted_nullification_over_system_replacement, conventional).
narrative_ontology:cs_reference_frame('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', constitutional_equality_floor_across_personal_laws).
narrative_ontology:cs_drift_state('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', post_shayara_bano_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4dbedf8-5b91-4f3a-8612-ab0e3a3e12a5', '').
narrative_ontology:cs_kernel_id(marriage_authority__gender_rights_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, communal_religious_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, male_lineage_households).
narrative_ontology:constraint_beneficiary(marriage_authority__gender_rights_reading, women_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__gender_rights_reading, ordinary_minority_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the councils, boards, and clerical offices that solemnize marriages, authorize dissolutions, and settle maintenance and inheritance disputes inside the community. Their jurisdiction and livelihood depend on families bringing these questions to them rather than to civil courts. After each court decision narrowing a practice they issue guidance reaffirming communal norms and mobilize political protection where challenged. Leaving the role would mean surrendering the office's entire reason for existence.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, communal_religious_authorities, agenda_setter,
    organized, generational, identity_locked, national).

% Heads of extended households who, at marriage-dissolution events, receive the material effects: a husband ending a marriage unilaterally sheds support obligations onto the wife's natal family, and widows and daughters are stepped aside in property division in favor of male heirs. They can settle disputes informally with cash payments that close the question without creating precedent, shifting form whenever formal rules tighten. Their standing inside the community rests on these arrangements continuing.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, male_lineage_households, beneficiary,
    powerful, biographical, arbitrage, local).

% Live under the community's family norms: they can be divorced at another's will, lose maintenance after a short prescribed period, and inherit little or nothing compared to brothers. Economic life runs through the marital household, so leaving means losing children, income, and community at once. A civil-code route exists on paper but carries severe social and economic costs. Some petition courts with advocates' help; most negotiate privately or endure.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_within_patriarchal_personal_law, payer,
    powerless, biographical, trapped, national).

% Lawyers, NGO staff, and scholars who locate affected women, prepare constitutional challenges, and argue them through the courts. Their caseload, funding, and public standing depend on the contest staying open; a comprehensive legislative settlement would dissolve their mandate. They spend heavily on each case and absorb accusations of outsidedness; between cases they maintain the coalitions, documentation, and petitioner pipelines that keep the issue alive. Their professional identity is fused with the contest itself.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, women_rights_advocates, beneficiary,
    organized, biographical, identity_locked, national).

% Hears the challenges, weighs religious freedom against equality guarantees, and issues the decisions that void specific practices or extend entitlements such as post-divorce maintenance. Each decision expands the court's reach into family life and becomes precedent that lowers the cost of the next case. Bound by precedent, docket discretion, and the political firestorm each major ruling ignites.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Men and women who hold no office and run no household line but carry the reputation costs of every public contest: each court battle casts the community as backward or besieged, tightening scrutiny on everyone. They mostly want workable family arrangements and quiet lives, and they pay a loyalty tax in stigma whichever side wins a given round.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, ordinary_minority_community_members, payer,
    moderate, biographical, constrained, national).

% Holds formal power to replace the whole patchwork with a common family code but has not exercised it since the mid-1980s, when an attempt to trim post-divorce maintenance triggered enough backlash to freeze the file. Every party assesses the electoral cost of touching family law and declines; the file stays closed while courts proceed case by case. Absence from the conversation is a choice, not a barrier.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, national_legislature, excluded,
    institutional, biographical, mobile, national).

% Review the country's family-law record under equality conventions, issue concluding observations urging uniform standards, and receive shadow reports from advocacy groups. Their leverage is reputational; they attend the contest from outside and press periodically without enforcement power of their own.
narrative_ontology:constraint_stakeholder(marriage_authority__gender_rights_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__gender_rights_reading, male_lineage_households).
narrative_ontology:fixing_cost_class(marriage_authority__gender_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal self-governance of family formation: communities maintain jurisdiction over marriage, dissolution, maintenance, and succession, preserving religious continuity and minority boundary-maintenance against a majoritarian family code, with disputes resolved inside community institutions rather than civil ones.
% TRANSFER_FUNCTION: Moves security, maintenance, and property entitlements from women at marriage-dissolution events to male lineage households; moves interpretive authority and jurisdictional livelihoods to communal religious officeholders; and, as the contest persists, moves standing, funding, and moral authority to the advocacy sector that prosecutes it.
% ABSENT_VOICES: Ordinary community women who live the practices but neither petition nor govern — their consent is presumed by communal elites and urban advocates alike. Divorced women who accepted informal cash settlements rather than litigate are invisible to the case-by-case record. The national legislature, which could speak for comprehensive settlement, has kept the file closed since the mid-1980s rollback. Intra-community dissenting theologians, who could authorize reform from inside, are marginal to both the defense and the challenge.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, divorce, maintenance, and inheritance would reorganize around civil codes; communal councils would lose their family-law jurisdiction and livelihood; lineage households would lose the obligation-shedding and property preference they currently collect; the advocacy sector would lose its mandate; and ordinary members would lose the boundary-maintenance function they partly rely on. Every seated party's position depends on the arrangement continuing in some form.
% FOUNDING_PROBLEM: Communal survival under colonial codification and majoritarian pressure: preserving religiously authorized family law as the boundary at which the community reproduces itself, so that family formation, dissolution, and succession remain governed by the community's own norms rather than absorbed into the majority's code.
% FOUNDING_PROBLEM_CORROBORATION: Communal authorities attest the problem is live, from a self-interested seat. Corroboration from outside the benefiting parties: the constitutional litigation record (the 1985 maintenance ruling and its legislative rollback, the 2001 extension of maintenance liability, the 2017 voiding of unilateral divorce) documents concrete harms and concrete protections independent of communal testimony; treaty-body concluding observations attest the equality deficit from outside the national contest; and intra-community dissenting theologians attest that the gatekept practices lack scriptural consensus, undermining the claim that communal survival requires them. No attesting source is neutral, but the litigation and treaty records sit outside the beneficiary set.
narrative_ontology:disappearance_verdict(marriage_authority__gender_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__gender_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__gender_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__gender_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__gender_rights_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__gender_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__gender_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__gender_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.74: judicial strikes trimmed the edges (unilateral divorce voided in form, maintenance extended past the prescribed period), but the core flows — property exclusion, gatekept settlement, and de facto continuation of instant divorce through informal channels — remain intact, so the reading's assessment stays high with a slow downward drift (0.84 to 0.74 across the interval). Suppression is authored at 0.82 and is a raw structural property, unscaled by power or scope: enforcement migrated from legal form into social coercion as formal instruments fell, and holding the line now requires continuous community-level sanction, which the rising suppression_requirement series tracks. Theater_ratio rises from 0.20 to 0.48: women's cells, consultation boards, and unenforceable contract clauses proliferate as performative responses to each reform shock — a symptom of defensive maintenance, not the classification test. Accessibility_collapse is 0.62: civil-code and migration alternatives exist on paper but collapse substantially under social and economic pressure; this is a constructed arrangement, not a natural law, so the value sits well below mountain range. Resistance is 0.58: sustained litigation and intra-community dissent meet organized counter-mobilization. Coalition check: the classic coalition path for powerless agents is blocked twice over — gatekeepers control the communication and economic channels that intra-class coalition would need, and the cross-class coalition with advocates runs on incentives that diverge (advocate mandate renewal favors contest persistence over settlement), so coalition power remains latent rather than effective. The measurement series run on one shared six-point grid so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   Four seats should compute differently. From the communal_religious_authorities seat the arrangement is genuine coordination they administer — communal continuity, minority boundary-maintenance — and each judicial strike is illegitimate intrusion; their identity_lock means exit is unthinkable without dissolving the office itself. From the women_within_patriarchal_personal_law seat the same structure operates as enforced extraction with no affordable exit. From the women_rights_advocates seat the structure is an adversary that nonetheless feeds them: every victory narrows the mandate that funds the next case, a parasitic-symbiosis the engine should surface as a beneficiary seat attached to a snare. From the constitutional_judiciary seat the arrangement is a docket of tractable problems — each ruling delivers a remedy, expands precedent, and defers the systemic question. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Communal_religious_authorities and male_lineage_households are declared beneficiaries with identity-locked and arbitrage-grade positions respectively, placing them near the beneficiary pole (low d, damped or inverted effective extraction). Women_within_patriarchal_personal_law are the declared victims with trapped exit, placing them near the full-target pole (high d, amplified effective extraction). Women_rights_advocates are declared beneficiaries on the persistence-rent ground, so the derivation places them near the subsidized end; the commentary records the caveat that they simultaneously expend heavily against the arrangement's operation — a same-atom nuance (authorities and advocates both hold the organized power atom) that per-atom directionality overrides cannot express, which is why no override is authored and the nuance lives here and in the omega instead. Constitutional_judiciary, ordinary_minority_community_members, national_legislature, and international_treaty_bodies fall to canonical fallbacks for their power atoms; none is declared in the beneficiary or victim arrays because none collects the arrangement's material flows.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — communal survival through family-law autonomy — is contested rather than dead: communal authorities attest it live, while the litigation record and treaty observations attest that the protective function has been substantially captured by gatekeeping interests. Because status is contested and the disappearance verdict is world_rearranges, the mismatch consumer (dead-status plus rearranges-verdict) does not fire; this is not a zombie arrangement but a live one whose protective shell is partly theatrical. The rising theater series (0.20 to 0.48) marks early decay symptoms inside the snare shell: if gatekeeping ever collapses, expect the defensive apparatus (boards, cells, consultation rituals) to persist as inertial performance before anyone dismantles it — a snare-to-piton drift path worth watching. Mandatrophy is therefore not resolved; the arrangement's mandate is disputed, not outlived. The classification prevents mislabeling in both directions: reading the arrangement as pure coordination (the communal-autonomy move) would erase the victim set; reading the reform route as pure liberation (this reading's own rhetoric) would erase the backlash and mandate-rent flows the measurements register.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the gender_rights_reading of the marriage_authority kernel; is the snare verdict a property of the standing arrangement or of this reading''s victim-set and benefit-structure declarations?',
    'Generate the four sibling files (communal_autonomy, federalist_millet, secularist, judicial_harmonization) over the shared referent and compare per-seat outputs; divergence localized to victim-set membership and epsilon confirms reading-indexed classification rather than measurement error.',
    'If the communal_autonomy file authors the same practices with no victims and low epsilon, the corpus gains a controlled contrast pair; any cross-reading aggregation must weight by reading rather than average, and the snare verdict travels only with this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Classification is indexed to the gender-rights reading of the marriage-authority kernel, not to the topic absolutely.').

omega_variable(
    advocate_seat_net_position,
    'Are women''s-rights advocates net collectors from the contest''s persistence (standing, funding, mandate rents) or net spenders (litigation costs, backlash exposure, career risk)?',
    'Organizational financial records and career-flow data across a settlement episode, e.g. advocacy-sector funding and staffing trajectories after the 2017 unilateral-divorce ruling; if organizations contract after major wins, expenditure dominates collection.',
    'Flips the advocate seat between subsidized-beneficiary and constrained-payer; determines whether the contest registers as self-sustaining regardless of outcomes, which bears on the snare-versus-tangled-rope boundary at the advocacy seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocate_seat_net_position, empirical, 'Net structural position of the advocacy sector relative to the contest it runs.').

omega_variable(
    backlash_cost_incidence,
    'Who absorbs the costs of each judicial strike against a practice — do gatekeeping institutions adapt while ordinary members and unrepresented women pay in tightened informal enforcement?',
    'Panel data on informal divorce prevalence, community sanction intensity, and maintenance-compliance rates before and after major rulings.',
    'Determines whether the measured extraction decline is a real transfer to women or displacement into less observable channels; if displacement dominates, the falling epsilon trajectory overstates reform and the standing arrangement is more entrenched than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_cost_incidence, empirical, 'Incidence of backlash costs generated by the reform route this reading pursues.').

omega_variable(
    informal_enforcement_visibility,
    'Does the rising suppression series measure genuine enforcement intensification or a visibility artifact as enforcement migrates from legal form into social coercion?',
    'Triangulate court records, ethnographic studies, and community surveys on sanction practice across the interval; look for discontinuities at the moments formal instruments were struck down.',
    'High: if informal enforcement exceeds the measured curve, the arrangement is more suppressive than the authored scalar shows and the snare verdict strengthens; if the rise is artifactual, suppression is flat and the enforcement-migration narrative is wrong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_visibility, empirical, 'Whether the suppression trajectory reflects real coercive intensification or measurement drift.').

omega_variable(
    protective_shell_capture,
    'Is the arrangement''s minority-protection function still live for ordinary community members, or has gatekeeping fully captured the protective shell?',
    'Compare welfare and autonomy outcomes for members who opt into civil family codes against those who remain under communal norms, controlling for socioeconomic status; genuine protection predicts comparable or better outcomes under communal norms for at least a subgroup.',
    'If protection is captured, the coordination story loses its last non-extractive leg and the founding-problem status resolves toward dead; if protection is real for some members, a hybrid coordination-plus-extraction reading gains ground at the story level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_shell_capture, conceptual, 'Whether the minority-protection rationale still does protective work or merely licenses gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__gender_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ma_gender_rights_tr_t0, marriage_authority__gender_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ma_gender_rights_tr_t8, marriage_authority__gender_rights_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(ma_gender_rights_tr_t16, marriage_authority__gender_rights_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(ma_gender_rights_tr_t24, marriage_authority__gender_rights_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(ma_gender_rights_tr_t32, marriage_authority__gender_rights_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(ma_gender_rights_tr_t40, marriage_authority__gender_rights_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(ma_gender_rights_be_t0, marriage_authority__gender_rights_reading, base_extractiveness, 0, 0.84).
narrative_ontology:measurement(ma_gender_rights_be_t8, marriage_authority__gender_rights_reading, base_extractiveness, 8, 0.82).
narrative_ontology:measurement(ma_gender_rights_be_t16, marriage_authority__gender_rights_reading, base_extractiveness, 16, 0.8).
narrative_ontology:measurement(ma_gender_rights_be_t24, marriage_authority__gender_rights_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(ma_gender_rights_be_t32, marriage_authority__gender_rights_reading, base_extractiveness, 32, 0.76).
narrative_ontology:measurement(ma_gender_rights_be_t40, marriage_authority__gender_rights_reading, base_extractiveness, 40, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(ma_gender_rights_su_t0, marriage_authority__gender_rights_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(ma_gender_rights_su_t8, marriage_authority__gender_rights_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(ma_gender_rights_su_t16, marriage_authority__gender_rights_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(ma_gender_rights_su_t24, marriage_authority__gender_rights_reading, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(ma_gender_rights_su_t32, marriage_authority__gender_rights_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(ma_gender_rights_su_t40, marriage_authority__gender_rights_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__gender_rights_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__gender_rights_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% The colloquial 'personal law debate' is one label over five structurally distinct claims about the marriage_authority kernel; per the epsilon-invariance principle they are authored as separate linked stories. This file is the gender-rights member: it targets the specific practice bundle (unilateral divorce, maintenance cutoffs, property exclusion) rather than the system-level authority structure, and it authors the only victim set composed of the women living under the practices. Sibling files share the referent (the standing arrangement) and author different reading-indexed values: communal_autonomy expects low epsilon with no victim set, federalist_millet expects a protective-coordination profile, secularist expects a transitional scaffold with a sunset, judicial_harmonization expects a mixed floor-imposition profile. Downstream coupling: rulings authored under this reading become the doctrinal bricks judicial_harmonization lays, and the political shocks to which secularist and federalist positions react; communal_autonomy sits upstream as the arrangement this reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
