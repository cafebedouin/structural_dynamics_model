% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: Post-Manifesto Monogamous Covenant Discipline (Endogenous-Revelation Reading)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   marriage_commitment_legitimacy: the endogenous_reinterpretation_reading,
 *   which holds the 1890 Manifesto to be genuine prophetic revelation — God
 *   commanded the reversal of plural marriage to preserve the Church for
 *   higher purposes. The constraint under this reading is the monogamous
 *   covenant discipline that bound Latter-day Saint membership thereafter:
 *   acceptance of the Manifesto as divine direction, administered through
 *   temple recommends and ecclesiastical councils, with the pre-1890 practice
 *   reinterpreted as a completed stage of covenant history rather than a
 *   defeated doctrine. On this reading the arrangement's extractiveness is
 *   low: the costs were real but covenant-integrated sacrifice, front-loaded
 *   in the 1890-1910 transition; federal pressure figures as the catalyst
 *   Providence used, not the cause; and theological continuity is preserved
 *   by reframing monogamy as the new covenant stage. The epsilon referent is
 *   the standing post-Manifesto arrangement assessed by this reading's own
 *   lights — not the arrangement the sibling readings would describe.
 *   CONSTRAINT FAMILY: this file is one of three readings of the same kernel.
 *   The sibling files instantiate different constraints with different
 *   epsilon: the exogenous_override_reading describes a coerced capitulation
 *   (high extraction — duress-driven, with identifiable cost-bearers and
 *   unchanged doctrine under pressure); the hybrid_pragmatic_reading
 *   describes strategic adaptation managed through scope ambiguity
 *   (intermediate extraction — prophetic authority deployed as a
 *   crisis-management instrument). The epsilon values differ because the
 *   readings locate causation and authorship differently; they are linked
 *   through network.affects_constraints and must never be averaged into one
 *   story. KEY AGENTS (by structural relationship): -
 *   first_presidency_and_quorum: agenda-setting beneficiary
 *   (institutional/identity_locked) — receives and interprets the revelation;
 *   prophetic succession legitimacy is vindicated by member acceptance -
 *   latter_day_saint_membership: coordinated beneficiary
 *   (organized/constrained) — adopts the monogamous covenant; gains legal
 *   security and institutional continuity - plural_marriage_families:
 *   cost-bearing payers (moderate/trapped) — dissolve households, endure
 *   prosecution, narrate the cost as commanded sacrifice -
 *   fundamentalist_dissenters: excluded payers (organized/constrained) —
 *   reject the reading, bear discipline, exit into schismatic communities -
 *   federal_government: external observer (institutional/analytical) —
 *   supplies the pressure this reading treats as catalyst, not cause
 *
 * KEY AGENTS:
 *   - first_presidency_and_quorum: agenda-setting beneficiary (institutional/identity_locked) — administers the covenant discipline; their revelatory authority is vindicated by member acceptance of the reversal
 *   - latter_day_saint_membership: coordinated beneficiary (organized/constrained) — accepts the Manifesto as divine direction; gains a legally secure church and uninterrupted temple access
 *   - plural_marriage_families: cost-bearing payers (moderate/trapped) — dissolve or conceal plural households, absorb prosecution and stigma, embed the cost in a sacrificial theological frame
 *   - fundamentalist_dissenters: excluded payers (organized/constrained) — hold the older marriage commandments binding, face discipline and excommunication, exit into isolated settlements
 *   - federal_government: external observer (institutional/analytical) — prosecutes, disincorporates, and conditions statehood; under this reading its pressure is the occasion of revelation, not its cause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.45).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "Post-Manifesto Monogamous Covenant Discipline (Endogenous-Revelation Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'abaef22b-e61a-4df2-ba72-ac07dfb087b4').
narrative_ontology:cs_kernel_codification('abaef22b-e61a-4df2-ba72-ac07dfb087b4', fixed_text).
narrative_ontology:cs_authority_grounding('abaef22b-e61a-4df2-ba72-ac07dfb087b4', lineage).
narrative_ontology:cs_interpretation_layer_present('abaef22b-e61a-4df2-ba72-ac07dfb087b4').
narrative_ontology:cs_reading_relation('abaef22b-e61a-4df2-ba72-ac07dfb087b4', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('abaef22b-e61a-4df2-ba72-ac07dfb087b4', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('abaef22b-e61a-4df2-ba72-ac07dfb087b4', foundational, manifesto_is_genuine_divine_command).
narrative_ontology:cs_axiom_status(manifesto_is_genuine_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('abaef22b-e61a-4df2-ba72-ac07dfb087b4', manifesto_is_genuine_divine_command, theological).
narrative_ontology:cs_axiom('abaef22b-e61a-4df2-ba72-ac07dfb087b4', foundational, prophetic_redirection_supersedes_prior_command).
narrative_ontology:cs_axiom_status(prophetic_redirection_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('abaef22b-e61a-4df2-ba72-ac07dfb087b4', prophetic_redirection_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('abaef22b-e61a-4df2-ba72-ac07dfb087b4', secondary, monogamy_is_new_covenant_stage).
narrative_ontology:cs_axiom_status(monogamy_is_new_covenant_stage, holdable).
narrative_ontology:cs_axiom_grounding('abaef22b-e61a-4df2-ba72-ac07dfb087b4', monogamy_is_new_covenant_stage, theological).
narrative_ontology:cs_reference_frame('abaef22b-e61a-4df2-ba72-ac07dfb087b4', living_oracle_covenant_continuity).
narrative_ontology:cs_drift_state('abaef22b-e61a-4df2-ba72-ac07dfb087b4', contemporary, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('abaef22b-e61a-4df2-ba72-ac07dfb087b4', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, latter_day_saint_membership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, first_presidency_and_quorum).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_marriage_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, manifesto_as_genuine_revelation).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, living_prophetic_authority).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, monogamy_new_covenant_stage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive, publish, and interpret the revelation ending new plural marriages; administer the covenant discipline through temple-recommend interviews and disciplinary councils; issue clarifying declarations when ambiguity invites abuse. Before 1890 many of them lived in hiding or served prison terms under anti-bigamy enforcement; after 1890 their authority depends on members accepting the reversal as God's word. Leaving the position would mean repudiating the office and the chain of revelation that constitutes it.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, first_presidency_and_quorum, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, first_presidency_and_quorum, beneficiary).

% Accept the Manifesto as divine direction and reorganize domestic life around monogamous covenant marriage. They gain a legally recognized church, uninterrupted temple access, and relief from prosecution exposure. The cost is reconciling the reversal with decades of teaching that plural marriage was eternally binding — a reconciliation this reading supplies by staging covenant practice across dispensations. Exit means forfeiting community, covenants, and kinship networks concentrated in the Intermountain West.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, latter_day_saint_membership, beneficiary,
    organized, generational, constrained, continental).

% Households sealed under the pre-1890 practice. Husbands dissolve or conceal second households; some serve prison terms for unlawful cohabitation; wives become sole providers and raise children under stigma. Their sealings, property, and kinship are embedded in the community, so exit would sever what they hold eternal. They narrate the cost as sacrifice required by God through the Prophet, and most remain in full fellowship.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, plural_marriage_families, payer,
    moderate, biographical, trapped, regional).

% Members — including two apostles who resigned rather than endorse the tightening enforcement — who hold the earlier marriage commandments to be still binding and the reversal uninspired or improperly administered. They face disciplinary councils, loss of temple privileges, and excommunication; some relocate to isolated settlements in Mexico, Canada, and the desert borderlands to continue the older practice. Their interpretation is barred from the authorized conversation inside the church.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters, excluded).

% Congress and the federal courts criminalize plural marriage, disincorporate the church, escheat its property, and imprison practitioners; statehood for Utah is conditioned on abandonment of the practice. Under this reading their pressure is the occasion Providence used to deliver the reversal — an instrument in the sequence, not its author. They take no part in administering the covenant discipline.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, first_presidency_and_quorum).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Re-founds the covenant community's marriage practice on a single revealed standard after the sovereign state made the prior practice legally untenable: one marriage discipline, uniformly administered, preserves corporate continuity, temple access, and communal identity under conditions that otherwise pointed to disincorporation and asset seizure.
% TRANSFER_FUNCTION: Moves domestic arrangement and obedience from the membership — concentrated on pre-existing plural households, who dissolve or conceal them — toward institutional preservation and the vindication of prophetic authority; moves legal recognition and legitimacy from a criminalized practice to a state-conforming one.
% ABSENT_VOICES: Plural wives whose sealings were set aside rarely appear as independent voices in the decision record; their accounts survive in diaries, court files, and family recollection rather than in the councils that administered the reversal. Dissenting holders of the older reading were excluded from the authorized conversation — their objection is documented in resignation letters and disciplinary minutes, delivered from outside the room. Both would contest the framing of cost as freely chosen sacrifice.
% DISAPPEARANCE_RATIONALE: If the covenant discipline vanished overnight and plural marriage resumed, the church would immediately collide again with federal criminal statutes, disincorporation, and asset escheatment; temple policy, domestic arrangements across thousands of households, and the institution's legal standing would all rearrange at once. The arrangement's disappearance is unthinkable for the seated parties precisely because their current positions depend on it.
% FOUNDING_PROBLEM: How a covenant community bound to a revealed marriage practice survives when the sovereign state criminalizes that practice — answered, on this reading, by fresh revelation that supersedes the earlier command for higher purposes rather than by surrender or dissolution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the federal record itself: congressional antipolygamy legislation, the Edmunds-Tucker proceedings, court dockets of cohabitation prosecutions, and the conditional grant of statehood all attest the existential character of the crisis independently of church testimony. Academic historians of American religious law, working outside the tradition, corroborate both the severity of the crisis and the completion of the transition. No source outside the benefiting parties attests that the crisis remains live; the persistence of the arrangement past its founding problem is visible in the record itself.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because on this reading the arrangement's flows are dominated by coordination benefit — institutional survival, legal security, covenant continuity — while the costs (dissolution of plural households, prosecution exposure, disciplinary exclusion of dissenters) are real but bounded, front-loaded in the 1890-1910 transition, and integrated theologically as sacrifice rather than taken as rent. Suppression (0.45) reflects genuine enforcement machinery — temple-recommend withdrawal, disciplinary councils, excommunication of post-Manifesto practitioners — that peaked around the 1904-1908 settlement and then normalized into routine covenant administration. Theater is low (0.12) because on this reading the function performed is the function claimed: the revelation is genuine, so enforcement defends a real covenant rather than a cover story; the modest rise during the Smoot-hearing era tracks public testimony performance, not structural hollowness. Accessibility_collapse (0.58) is moderate: within the mainstream body the alternative (continuing plural marriage) collapsed almost completely after the Second Manifesto, but physical exit into fundamentalist communities remained available at high cost, so alternatives did not vanish outright. Resistance (0.42) records the real minority resistance — two apostles resigned rather than comply, colonies in Mexico and Canada continued the older practice, and a schismatic movement crystallized. The identity_coordination typing is genuine here, not a cover story: the constraint's primary function is covenant boundary maintenance — defining who the covenant people are under a redirected marriage standard — and the FNL gaming risk (identity framing laundering extraction) is checked by the low theater ratio and the bounded, transition-concentrated cost profile. The temporal series run on one shared grid (1890-1930, eight points) so every metric is authored at every examined time point: extractiveness declines as the transition completes, suppression rises to the enforcement peak then decays, theater bumps during the public-testimony era and settles low.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the first presidency seat the arrangement is fulfilled revelation: the same structure that cost them prison and exile before 1890 vindicates their office after it, so the seat reads low extraction and high legitimacy. From the plural-family seat the identical structure operated as forced domestic dissolution under covenant obligation — real costs borne without compensating return in that generation, tempered only by the theological frame that converts loss into sacrifice. From the dissenter seat the structure is an unauthorized barrier excluding their reading of the same revelations. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the membership and leadership seats toward the beneficiary end of d: the membership receives legal security and continuity; the leadership receives vindicated prophetic succession legitimacy — the reading's own account of where the gains land (recorded in gain_flow). The leadership seat is nonetheless blended: its members personally bore the persecution-era costs and staked their authority on the reversal's acceptance, so their realized position sits nearer symmetry than a pure collector's. The plural-family seat derives high d from the payer role amplified by trapped exit — sealings, property, and kinship make exit self-destructive. Dissenters derive high d from their payer and excluded position, tempered by the constrained-but-real exit they ultimately exercised. The federal government takes no directional position into the covenant structure: under this reading its pressure is catalyst, not cause, and it neither collects from nor pays into the arrangement. Divine authority — the reading's ultimate beneficiary-position — is carried as vindicated proposition and axiom, not as an agent feeding the directionality arithmetic, because a doctrine collects no rents and must not enter the chi computation as if it did.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — resolve the existential collision between covenant practice and federal law — was fulfilled: the transition completed, the legal crisis passed, and the arrangement persists as the standing covenant baseline rather than as an emergency measure. This produces a formal R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) that the consumer cross-checks against the theater path: theater is low (0.12) and the enforcement machinery performs its stated covenant function, so the mismatch resolves as fulfilled-transition rather than zombie persistence. The classification prevents mislabeling in both directions: reading the persistence as mandate-atrophy would misread covenant continuity as piton decay; reading the low extraction as absence of any cost-bearing would erase the plural families' real losses. Mandatrophy_resolved is deliberately not declared because the constraint's function did not outlive itself — it transformed; the R5 fields carry the precise signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Which reading of the kernel marriage_commitment_legitimacy correctly describes the 1890 reversal — genuine divine command (this file), coerced capitulation (exogenous_override_reading), or strategic adaptation (hybrid_pragmatic_reading)?',
    'Not resolvable by this file alone; resolution requires comparative analysis across the three sibling stories plus historical-theological assessment of the revelation claim against the documentary record (Woodruff''s journal accounts, the pre-Manifesto deliberations, the timing of federal escalation).',
    'Each sibling reading assigns a different epsilon to the same historical arrangement: the exogenous reading raises extraction sharply (duress-driven, identifiable cost-bearers), the hybrid reading redistributes it (scope ambiguity as the extraction vehicle). Classification of the standing arrangement flips across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame omega: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    causation_locus_disagreement,
    'Where do the readings locate the cause of the reversal — divine command with federal pressure as catalyst (this reading), federal coercion as cause (exogenous), or institutional strategy managing an exogenous crisis (hybrid)?',
    'Structural comparison of the three stories'' beneficiary and cost-bearing declarations and enforcement data; the disagreement is located precisely at the causation-authorship node, which each reading wires differently.',
    'If causation relocates from divine command to coercion, the cost-bearing seats recompute as targets of enforced capitulation and effective extraction rises well above the authored 0.28; if it relocates to strategy, scope ambiguity becomes the load-bearing extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_locus_disagreement, conceptual, 'The specific structural element on which the sibling readings disagree: the locus of causation.').

omega_variable(
    sacrifice_vs_extraction_status,
    'Do the costs borne by plural-marriage families constitute covenant sacrifice integrated into the reading''s frame, or uncompensated extraction that the theological gloss merely relabels?',
    'Welfare and outcome comparison of dissolved plural households against matched monogamous households of the same period and region; attestation analysis of the families'' own journals against the institutional record.',
    'If uncompensated harm dominates, effective extraction for the trapped cost-bearing seat rises substantially and the arrangement computes nearer a hybrid with asymmetric burden; if the sacrifice integration holds, the low-extraction profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrifice_vs_extraction_status, empirical, 'Whether the reading''s sacrifice framing accurately renders the paying seat''s cost structure.').

omega_variable(
    revelation_genuineness_undecidability,
    'Can the genuineness of the revelation be adjudicated by any observable available to structural analysis, or is it indexed to faith commitment such that this reading''s low extraction is stable only while the frame holds?',
    'None available outside commitment frameworks; the question resolves only within a tradition''s own tests of revelation. Frame-collapse events (mass disaffection over the reversal) would be the observable symptom of frame failure.',
    'While the frame holds, this story''s low extraction is stable; if the frame breaks for a population, that population''s seat migrates to the exogenous or hybrid reading''s constraint with its higher extraction profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revelation_genuineness_undecidability, conceptual, 'External undecidability of the genuineness premise on which this reading''s epsilon depends.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1890, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.13).
narrative_ontology:measurement_basis(marr_tr_t1896, observed).
narrative_ontology:measurement(marr_tr_t1902, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1902, 0.17).
narrative_ontology:measurement_basis(marr_tr_t1902, observed).
narrative_ontology:measurement(marr_tr_t1908, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1908, 0.18).
narrative_ontology:measurement_basis(marr_tr_t1908, observed).
narrative_ontology:measurement(marr_tr_t1914, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1914, 0.16).
narrative_ontology:measurement_basis(marr_tr_t1914, observed).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.14).
narrative_ontology:measurement_basis(marr_tr_t1920, observed).
narrative_ontology:measurement(marr_tr_t1926, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1926, 0.13).
narrative_ontology:measurement_basis(marr_tr_t1926, observed).
narrative_ontology:measurement(marr_tr_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement_basis(marr_tr_t1930, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.46).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.43).
narrative_ontology:measurement_basis(marr_be_t1896, observed).
narrative_ontology:measurement(marr_be_t1902, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1902, 0.45).
narrative_ontology:measurement_basis(marr_be_t1902, observed).
narrative_ontology:measurement(marr_be_t1908, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1908, 0.36).
narrative_ontology:measurement_basis(marr_be_t1908, observed).
narrative_ontology:measurement(marr_be_t1914, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1914, 0.33).
narrative_ontology:measurement_basis(marr_be_t1914, observed).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.31).
narrative_ontology:measurement_basis(marr_be_t1920, observed).
narrative_ontology:measurement(marr_be_t1926, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1926, 0.29).
narrative_ontology:measurement_basis(marr_be_t1926, observed).
narrative_ontology:measurement(marr_be_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1930, 0.28).
narrative_ontology:measurement_basis(marr_be_t1930, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.34).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1896, 0.38).
narrative_ontology:measurement_basis(marr_su_t1896, observed).
narrative_ontology:measurement(marr_su_t1902, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1902, 0.46).
narrative_ontology:measurement_basis(marr_su_t1902, observed).
narrative_ontology:measurement(marr_su_t1908, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1908, 0.63).
narrative_ontology:measurement_basis(marr_su_t1908, observed).
narrative_ontology:measurement(marr_su_t1914, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1914, 0.56).
narrative_ontology:measurement_basis(marr_su_t1914, observed).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.51).
narrative_ontology:measurement_basis(marr_su_t1920, observed).
narrative_ontology:measurement(marr_su_t1926, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1926, 0.48).
narrative_ontology:measurement_basis(marr_su_t1926, observed).
narrative_ontology:measurement(marr_su_t1930, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 1930, 0.45).
narrative_ontology:measurement_basis(marr_su_t1930, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Manifesto's legitimacy' covers three structurally distinct claims that measure differently. This file (endogenous_reinterpretation_reading) authors low epsilon because the arrangement is read as genuine divine coordination; the exogenous_override_reading authors high epsilon because the same arrangement is read as duress-driven capitulation with identifiable cost-bearers; the hybrid_pragmatic_reading authors intermediate epsilon with scope ambiguity as the extraction vehicle. The upstream/downstream structure runs from the revelation-authenticity question to the extraction question: whichever reading prevails on authorship determines the victim set and enforcement reading for the other two. Each story carries its own beneficiaries, cost-bearers, metrics, and classification; the family is linked exclusively through network edges, never by averaging.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
