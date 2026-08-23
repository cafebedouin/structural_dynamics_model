% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Deferred Restoration Blueprint (Performance-Only Reading)
 *   domain: religious/commitment_system
 *
 * SUMMARY:
 *   Within the performance_only reading, the Kodashim corpus — the talmudic
 *   orders detailing sacrificial procedure — is an archived operational
 *   blueprint whose subject matter cannot currently be performed: the altar
 *   is gone, and the rite resumes only at a messianic restoration no human
 *   action can bring about. The standing arrangement under contest is the
 *   apparatus built around that deferral: yeshiva tracks devoted to sacrifice
 *   law, restoration-movement projects reconstructing vessels and purity
 *   prerequisites, and a devotional economy in which study is framed as
 *   readiness rather than as performance. Legitimacy flows to the
 *   institutions that certify readiness, drawn against a future act they
 *   cannot deliver; devotion, tuition, and donor funds flow upward from those
 *   who accept the readiness frame. The claim and the metrics are independent
 *   authored facts: the reading itself presents the arrangement as faithful
 *   custodianship (its own cover story is preparation), while the metrics
 *   below describe what the deferral structure actually does — sustained
 *   collection against a payoff whose arrival is outside any participant's
 *   control. Per the epsilon referent rule, epsilon assesses THIS standing
 *   arrangement by this reading's own lights, not the restored-sacrifice
 *   state the reading endorses.
 *
 * KEY AGENTS:
 *   - messianic_preparation_institutions: agenda-setting administrators (institutional/mobile) — run the study tracks, certify readiness, collect tuition, donations, and interpretive authority
 *   - temple_restoration_movements: secondary collectors (organized/constrained) — convert the restoration premise directly into projects, donations, and public legitimacy
 *   - kodashim_devotees: primary bearers of cost (moderate/identity_locked) — allocate years of formation to tractates whose content cannot be practiced, receiving readiness certification as the return
 *   - lay_donor_communities: diffuse funders (organized/constrained) — sustain the apparatus financially and receive participatory meaning as partners in preparation
 *   - immediate_performance_activists: excluded challengers (organized/trapped) — attempt the rite now and are ruled out of bounds by the deferral logic itself
 *   - rival_reading_adherents: excluded internal dissenters (organized/analytical) — occupy the same texts under accounts in which the study is the act or the archive is memorial
 *   - academic_scholars_of_rabbinics: analytical observers (institutional/analytical) — document the corpus's transmission history and the apparatus built upon it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.8).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.62).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.8).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Deferred Restoration Blueprint (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '62abd32b-69fe-4c22-84a2-949fa98a112c').
narrative_ontology:cs_kernel_codification('62abd32b-69fe-4c22-84a2-949fa98a112c', fixed_text).
narrative_ontology:cs_authority_grounding('62abd32b-69fe-4c22-84a2-949fa98a112c', extraction).
narrative_ontology:cs_interpretation_layer_present('62abd32b-69fe-4c22-84a2-949fa98a112c').
narrative_ontology:cs_reading_relation('62abd32b-69fe-4c22-84a2-949fa98a112c', kodashim_corpus__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('62abd32b-69fe-4c22-84a2-949fa98a112c', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('62abd32b-69fe-4c22-84a2-949fa98a112c', foundational, sacrifice_resumes_only_in_messianic_restoration).
narrative_ontology:cs_axiom_status(sacrifice_resumes_only_in_messianic_restoration, holdable).
narrative_ontology:cs_axiom_grounding('62abd32b-69fe-4c22-84a2-949fa98a112c', sacrifice_resumes_only_in_messianic_restoration, theological).
narrative_ontology:cs_axiom('62abd32b-69fe-4c22-84a2-949fa98a112c', foundational, study_is_preparation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preparation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('62abd32b-69fe-4c22-84a2-949fa98a112c', study_is_preparation_not_performance, conventional).
narrative_ontology:cs_reference_frame('62abd32b-69fe-4c22-84a2-949fa98a112c', second_temple_operational_baseline).
narrative_ontology:cs_drift_state('62abd32b-69fe-4c22-84a2-949fa98a112c', post_return_nonrestoration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('62abd32b-69fe-4c22-84a2-949fa98a112c', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, temple_restoration_movements).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, kodashim_devotees).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_donor_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, lay_donor_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the yeshiva tracks, publication houses, and certification structures through which sacrifice law is taught as readiness training. Set the curriculum, define what counts as faithful engagement with the corpus, and collect tuition, donations, and interpretive authority in return. Their organizational assets — faculties, presses, donor networks — are portable across framings, so a doctrinal reframing would cost them prestige but not existence.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, mobile, global).

% Run the reconstruction projects — vessels, garments, purity logistics, red-heifer programs — that materialize the restoration premise. Convert the deferral frame directly into project funding, volunteer labor, and public visibility. Their public identity is welded to the restoration claim, so repositioning under a different account of the texts would dissolve the movement's reason for existing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, temple_restoration_movements, beneficiary,
    organized, generational, constrained, regional).

% Devote years of formation to tractates whose subject matter cannot be practiced, on the understanding that this study earns readiness for a restoration they expect within the tradition's own timeline. What flows to them is certification of readiness and standing as serious students; what flows from them is tuition, labor, and a lifetime allocation of attention. Leaving the track means recasting a self built around preparedness, forfeiting communal standing, and often severing the marriage and employment networks formed inside it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, kodashim_devotees, payer,
    moderate, generational, identity_locked, global).

% Fund the study tracks and restoration projects through recurring donations, receiving in return participatory identity as partners in preparation and assurance that the tradition's central rite remains alive in prospect. Their giving is voluntary and individually small, but stopping means withdrawing from a communal practice their peers continue, and the meaning they purchase is denominated in the same deferred restoration everyone else awaits.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_donor_communities, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__performance_only, lay_donor_communities, beneficiary).

% Attempt elements of the sacrificial rite now, at the historic site, treating the blueprint as immediately executable. The deferral frame rules them out of bounds — premature, conditionally unqualified, or provocatively dangerous — so their objection that waiting itself betrays the blueprint is never admitted into the tradition's internal conversation. Their attempts periodically force confrontations that the interpretive authorities must then manage.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, immediate_performance_activists, excluded,
    organized, biographical, trapped, regional).

% Occupy the same texts under different accounts — that the study is itself the act, or that the corpus is a memorial of what prayer and study replaced. Within communities governed by the preparation frame, their account of why the study matters is treated as a deviation rather than an alternative reading, and their devotional practice goes uncertified by the readiness apparatus. They contest the frame from inside the textual tradition without controlling its institutions.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, rival_reading_adherents, excluded,
    organized, civilizational, analytical, global).

% Document the corpus's transmission history, the post-destruction crisis that produced it, and the modern apparatus built upon it. They take no side in the doctrinal contest, publish analyses the contending parties read selectively, and supply the historical record against which claims about the founding problem and its present status can be checked.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, academic_scholars_of_rabbinics, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains uninterrupted intergenerational transmission of detailed sacrificial procedure in the absence of any functioning altar, and organizes dispersed communities around a shared calendar of anticipation — common texts, common curricula, common expectations about restoration.
% TRANSFER_FUNCTION: Moves tuition, recurring donations, scholarly labor, and devotional attention from devotees and donor communities to the institutions that certify readiness and control the interpretation of the corpus; moves legitimacy downward from the certifiers to the devotees as readiness credentials.
% ABSENT_VOICES: Three seats are outside the conversation. Immediate-performance activists would object that the deferral itself betrays the blueprint, but the frame rules their position out of bounds as premature. Rival-reading adherents would object that the preparation framing strips their devotional practice of standing, but they lack the institutional microphone. And the secular descendants of devotee families — those who left the track entirely — carry the uncounted cost of generations of allocated attention, and no mechanism records their assessment of what the allocation purchased.
% DISAPPEARANCE_RATIONALE: If the preparation apparatus vanished overnight, the institutions would lose their organizing purpose and revenue, restoration projects would halt for lack of certification and funding, devotee study tracks would empty as the readiness credential lost its issuer, and donor flows would redirect — the communities organized around anticipation would rearrange around whichever account of the texts survived, and the corpus itself would persist as archive rather than as lived program.
% FOUNDING_PROBLEM: The destruction of the Second Temple made the covenant's central rite suddenly impossible to perform. The arrangement was built to solve the crisis of discontinuity: to carry the complete operational knowledge of sacrifice through exile intact, so that nothing would be missing when performance became possible again.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis itself is corroborated from outside the benefiting parties: Josephus's account of the destruction and its aftermath, the internal evidence of Mishnaic redaction priorities, and Christian and Muslim chronicles documenting the site's loss and Jewish petitionary practice all attest that the rite ended and that its loss was mourned as catastrophic. But no external party attests that the present-day apparatus still serves that founding problem rather than reproducing itself: the claim that current study-and-preparation activity advances the original mandate is attested only by the institutions that collect from the activity, and the rival readings inside the tradition explicitly deny it. That asymmetry — corroborated origin, internally-attested-only continuation — is itself signal.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because the arrangement's return — certified readiness for an act only a messianic restoration can permit — is structurally undeliverable by the arrangement itself; every unit of devotion collected is collected against a payoff the payer cannot verify, hasten, or audit. Suppression (0.62) is real but non-coercive in form: it operates as interpretive gatekeeping, communal sanction, and identity formation rather than force, and it is authored as a raw structural property, unscaled by power or scope. Theater_ratio (0.56) reflects the growing share of activity shaped like rehearsal for an exercise that cannot begin — vessel reconstruction, purity logistics, staged liturgy — alongside genuinely transmissible scholarship. Accessibility_collapse (0.48) is moderate: exits exist (the rival accounts within the tradition, and departure from it altogether) but each requires reframing what a lifetime of devotion meant, which is precisely why the exit door is expensive rather than closed. Resistance (0.38) is persistent but muted — rationalist critique runs from Maimonides to the modern denominations, yet inside the host communities the deferral frame is rarely confronted head-on. The temporal series share one grid (points 0–60); all three metrics rise together: extraction accumulates as the apparatus professionalized, theater grows as rehearsal projects multiplied, and enforcement hardened as the rival accounts gained cultural ground — the suppression series is included because the story specifically traces that interpretive-boundary hardening, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the arrangement is custodianship it administers faithfully: the corpus is intact, transmission is unbroken, and the deferral is the tradition's own doctrine rather than a policy anyone chose — extraction is invisible from inside because the payoff is denominated in a currency (restoration) the seat sincerely expects. From the devotee seat, the same structure is a lifetime allocation whose return certificate (readiness) is issued by the very institutions collecting the tuition, redeemable only at an event neither party controls. The donor seat experiences a third structure: meaningful participation priced continuously, with the redemption date perpetually announced and never scheduled. The engine derives these divergences from the declared roles, power levels, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutions sit nearest the beneficiary pole: they collect the flows and set the interpretive rules, and their exit is mobile — they can reframe curricula, merge with rival accounts, or pivot missions without losing standing, because their asset is organizational, not biographical. The restoration movements are beneficiaries with less mobility: their brand is welded to the restoration premise, so they collect heavily but cannot reposition cheaply. Devotees sit nearest the target pole: maximal extraction, identity-locked exit — their formation, marriages, and self-concept are constituted inside the readiness frame, so the derivation places them at the full-target end regardless of their nominal communal standing. Donor communities are targets with partial subsidy: they pay continuously but receive identity goods, placing them mid-range rather than at the pole. The excluded seats carry no directional weight — their exclusion is itself part of the enforcement surface, since the deferral logic is what rules the immediate-performance activists out of bounds and what renders the rival accounts inadmissible as accounts of the same texts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — carrying an unusable blueprint through exile without losing it — was real, urgent, and largely solved: the corpus survived intact and transmissible. The mandatrophy question is whether the arrangement's mandate has outlived that function. Within the reading's own doctrine it cannot have: the mandate terminates only at restoration, so by design it never expires. Structurally, however, the preservation function completed long ago — the knowledge is archived, printed, and digitized — and what the apparatus now reproduces is the anticipation, not the blueprint. The snare classification prevents the two standard mislabelings: reading the arrangement as pure coordination (knowledge preservation) hides the asymmetric flow of devotion and funds toward certifiers; reading it as empty performance erases the sincere scholarship and the real continuity service it still performs. The classification also surfaces the drift hazard: if the rehearsal share keeps rising while the transmissible-scholarship share falls, the arrangement decays toward inertial performance maintained by the very interests its completion would dissolve — the deferral-interest omega is the early-warning instrument for that transition. The mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): the founding crisis is historically real, but whether the present apparatus still serves it is disputed even inside the tradition, and the arrangement's disappearance would visibly rearrange the communities organized around it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kodashim_corpus kernel — the performance_only reading, in which the corpus is an unoccupied husk awaiting physical resumption. Sibling readings (study_as_exercise: study IS the performance; substitution_archive: prayer and study permanently replaced sacrifice) instantiate different constraints with different epsilon values over the same texts. What would a sibling reading change structurally?',
    'Cross-reading comparison within the constraint family: under study_as_exercise the same study is the mitzvah itself and measured extraction collapses toward coordination cost; under substitution_archive the corpus is memorial record and extraction drops further still. The disagreement is located in a single structural element: whether occupation of the kernel requires physical performance.',
    'If the sibling readings are adopted as the operative account, this story''s high epsilon does not transfer — the victim class (misallocated devotion) dissolves because the devotion is redefined as the payoff. This story''s classification is valid only for the performance_only seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: one reading of a three-reading kernel; epsilon is reading-indexed.').

omega_variable(
    restoration_realizability,
    'Is the future state from which the arrangement draws legitimacy — resumed physical sacrifice — structurally reachable, or is its fulfillment condition unfalsifiable by design?',
    'Doctrinal analysis of the restoration conditions the reading itself specifies (prophetic signs, purity prerequisites, divine initiative) versus any condition human action could satisfy; comparative study of deferral structures in other restorationist traditions.',
    'If restoration is genuinely reachable (even if divinely gated), part of the measured extraction is option premium — payment for a real possibility. If the fulfillment condition is unreachable by construction, legitimacy derives from an unredeemable promissory note and the arrangement''s extraction is capture-grade with no terminal state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_realizability, conceptual, 'Whether the deferred payoff is a real option or an unfalsifiable promissory structure.').

omega_variable(
    deferral_interest_ambiguity,
    'Does the apparatus have a structural interest in the restoration remaining pending? Fulfillment would complete the mission and dissolve the mediating role of the institutions that certify readiness — does the arrangement therefore reproduce deferral?',
    'Institutional behavior analysis: track whether the apparatus celebrates milestones that shorten the deferral (a qualified red heifer, reconstructed vessels) or whether each near-milestone is followed by newly specified prerequisites that extend the preparation period.',
    'If institutions systematically extend prerequisites, the deferral is maintained for the sake of the deferral and extraction is deliberate capture. If milestones are pursued sincerely, the extraction is devotional overhead on a genuine (if unverifiable) expectation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_interest_ambiguity, empirical, 'Whether the fulfillment condition dissolving the mediator''s role produces structural deferral preference.').

omega_variable(
    sincerity_theater_boundary,
    'Is the rising share of rehearsal-shaped activity (vessel reconstruction, purity logistics, liturgical staging) sincere preparation or performative maintenance of an arrangement whose operative function cannot be exercised?',
    'Participant-observation and discourse analysis: distinguish activity undertaken as if it could become operative from activity whose audience is the community''s own assurance. Compare resource allocation between rehearsal projects and transmissible scholarship.',
    'If the rehearsal share is sincere preparation, theater_ratio overstates decay and the arrangement retains functional content. If it is maintenance aimed at the community rather than at restoration, the arrangement is drifting toward inertial performance — the piton signature — while retaining its extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_theater_boundary, empirical, 'Sincere rehearsal versus theatrical maintenance of an unexercisable function.').

omega_variable(
    identity_lock_composition,
    'How much of the devotees'' persistence is structural closure (communal sanction, sunk formation costs, marriage and employment networks inside the community) versus internalized fusion (self-concept constituted by readiness-certified devotion)?',
    'Post-exit trajectory study of those who leave the devotional track: if the sense of misallocated devotion and the pull of the preparation frame persist after all structural barriers are removed, a substantial share of the hold is internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests — leavers carry the frame with them, and coalition formation among devotees is far less likely than the raw exit data implies. If structural, removing communal sanction would release most of the withheld exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_composition, empirical, 'Structural versus internalized composition of the identity lock on devotees.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.32).
narrative_ontology:measurement(kodashim_perf_tr_t10, kodashim_corpus__performance_only, theater_ratio, 10, 0.36).
narrative_ontology:measurement(kodashim_perf_tr_t20, kodashim_corpus__performance_only, theater_ratio, 20, 0.4).
narrative_ontology:measurement(kodashim_perf_tr_t30, kodashim_corpus__performance_only, theater_ratio, 30, 0.44).
narrative_ontology:measurement(kodashim_perf_tr_t40, kodashim_corpus__performance_only, theater_ratio, 40, 0.48).
narrative_ontology:measurement(kodashim_perf_tr_t50, kodashim_corpus__performance_only, theater_ratio, 50, 0.52).
narrative_ontology:measurement(kodashim_perf_tr_t60, kodashim_corpus__performance_only, theater_ratio, 60, 0.56).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(kodashim_perf_be_t10, kodashim_corpus__performance_only, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(kodashim_perf_be_t20, kodashim_corpus__performance_only, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(kodashim_perf_be_t30, kodashim_corpus__performance_only, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(kodashim_perf_be_t40, kodashim_corpus__performance_only, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(kodashim_perf_be_t50, kodashim_corpus__performance_only, base_extractiveness, 50, 0.77).
narrative_ontology:measurement(kodashim_perf_be_t60, kodashim_corpus__performance_only, base_extractiveness, 60, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_perf_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(kodashim_perf_su_t10, kodashim_corpus__performance_only, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(kodashim_perf_su_t20, kodashim_corpus__performance_only, suppression_requirement, 20, 0.54).
narrative_ontology:measurement(kodashim_perf_su_t30, kodashim_corpus__performance_only, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(kodashim_perf_su_t40, kodashim_corpus__performance_only, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(kodashim_perf_su_t50, kodashim_corpus__performance_only, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(kodashim_perf_su_t60, kodashim_corpus__performance_only, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__study_as_exercise).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'study of the sacrifices' covers three structurally distinct arrangements that differ on whether the kernel is occupied (study_as_exercise), superseded (substitution_archive), or deferred (performance_only, this file). Each carries its own epsilon, victim structure, and classification; forcing one story to span all three would make epsilon observable-dependent, which the chi formula forbids. This upstream reading — the one whose deferral premise generates the highest extraction — influences the siblings' operating environment: the preparation apparatus supplies the institutional infrastructure through which the rival accounts must route their claims, and its enforcement surface defines what counts as admissible engagement with the same texts. Family members are linked bidirectionally via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
