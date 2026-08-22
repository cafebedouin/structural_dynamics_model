% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Residual Honor-Settlement Code in Fringe Niches (Drop Reading)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   This story instantiates the drop reading of the
 *   honor_settlement_legitimacy kernel: dueling declined across the
 *   nineteenth-century West but persisted as fringe practice among residual
 *   honor-culture adherents — German academic-fencing fraternities (the
 *   Mensur, fought under contested legality into the present), Balkan and
 *   Caucasus feud codes (including the Kanun's post-1990s Albanian revival),
 *   officer-corps and gentlemanly dueling into the early twentieth century,
 *   and the modified self-help violence of Southern American honor culture.
 *   The constraint modeled is the residual code itself as it operates inside
 *   those niches: a settlement mechanism for injuries host-state law does not
 *   recognize, maintained against criminalization by custodian communities,
 *   binding junior adherents through standing sanctions. The extractiveness
 *   referent is the standing arrangement — the niche code as actually
 *   practiced — assessed by this reading's own lights, which take the
 *   persistence as a live normative option rather than inert residue.
 *   Interval mapping: T0 = 1850 (criminalization consolidating across Western
 *   Europe, niches consolidating), T170 = 2020 (fencing-fraternity
 *   continuity, feud-code revival episodes). The claimed type and the metrics
 *   are authored independently: the claim is tangled_rope on structural facts
 *   — a genuine niche coordination function, asymmetric risk-bearing, active
 *   enforcement — while the metrics describe the arrangement's observed
 *   operation over the interval.
 *
 * KEY AGENTS:
 *   - honor_community_custodians: agenda-setting custodian tier (organized/identity_locked) — administers the code inside the niche: certifies challenges, appoints seconds, sanctions refusals, absorbs legal exposure
 *   - established_standing_holders: primary beneficiary (moderate/identity_locked) — collects standing from the community's willingness to answer challenges; rarely faces one
 *   - junior_honor_adherents: primary target (powerless/identity_locked) — bears injury risk and legal jeopardy as the price of admission to standing
 *   - duel_refusers: enforcement target (powerless/constrained) — bears sanction for refusal; most exit the niche, which is the sanction's function
 *   - duel_casualty_kin: inherited-cost bearers (powerless/generational) — inherit death, injury, and in feud branches a settlement obligation that can pass to children
 *   - state_legal_authorities: external enforcer of suppression (institutional/constrained) — its enforcement intensity sets the underground form the code takes; bears prosecution costs and toleration scandals
 *   - honor_community_women: excluded voice (powerless/generational) — bound by the code's consequences through kinship, seated in none of its forums
 *   - anti_dueling_campaigners: excluded voice (organized/mobile) — won the criminal prohibitions; their objection is inadmissible inside the code's own adjudication
 *   - honor_culture_ethnographers: analytical observer (analytical/analytical) — sees the whole structure across niches from no seat in any of them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.64).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.62).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Residual Honor-Settlement Code in Fringe Niches (Drop Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'e23ec827-8fc1-479b-923c-7f332c0645d6').
narrative_ontology:cs_kernel_codification('e23ec827-8fc1-479b-923c-7f332c0645d6', distributed).
narrative_ontology:cs_authority_grounding('e23ec827-8fc1-479b-923c-7f332c0645d6', lineage).
narrative_ontology:cs_interpretation_layer_present('e23ec827-8fc1-479b-923c-7f332c0645d6').
narrative_ontology:cs_reading_relation('e23ec827-8fc1-479b-923c-7f332c0645d6', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e23ec827-8fc1-479b-923c-7f332c0645d6', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('e23ec827-8fc1-479b-923c-7f332c0645d6', foundational, normative_repertoires_retain_live_alternatives).
narrative_ontology:cs_axiom_status(normative_repertoires_retain_live_alternatives, holdable).
narrative_ontology:cs_axiom_grounding('e23ec827-8fc1-479b-923c-7f332c0645d6', normative_repertoires_retain_live_alternatives, empirically_contingent).
narrative_ontology:cs_axiom('e23ec827-8fc1-479b-923c-7f332c0645d6', secondary, suppression_redirects_without_eliminating).
narrative_ontology:cs_axiom_status(suppression_redirects_without_eliminating, holdable).
narrative_ontology:cs_axiom_grounding('e23ec827-8fc1-479b-923c-7f332c0645d6', suppression_redirects_without_eliminating, empirically_contingent).
narrative_ontology:cs_reference_frame('e23ec827-8fc1-479b-923c-7f332c0645d6', persistent_niche_repertoire).
narrative_ontology:cs_drift_state('e23ec827-8fc1-479b-923c-7f332c0645d6', post_state_collapse_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('e23ec827-8fc1-479b-923c-7f332c0645d6', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_community_custodians).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, established_standing_holders).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, junior_honor_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duel_refusers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duel_casualty_kin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, junior_honor_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior members who administer the code inside the niche: they certify challenges, appoint seconds, train and roster answerers, sanction refusals, and absorb the niche's legal exposure. Fraternity comment-books, clan councils, and officers' courts are their instruments. Their own authority exists only insofar as the code operates; abandoning it would dissolve the standing they hold.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_community_custodians, agenda_setter,
    organized, generational, identity_locked, regional).

% Senior adherents whose reputations the code secures. They collect standing from the community's willingness to answer challenges and rarely face one themselves; the code's operation maintains the hierarchy they sit atop. Exit would forfeit standing accumulated over a career.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, established_standing_holders, beneficiary,
    moderate, biographical, identity_locked, regional).

% Young men for whom answering a challenge is the price of admission to standing. They bear the physical risk and the legal jeopardy; refusal costs them the standing they have no other route to. Within the niche they cannot decline without sanction, and leaving means abandoning the status they entered to gain.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, junior_honor_adherents, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, junior_honor_adherents, beneficiary).

% Men who declined challenges. They bear standing loss, sometimes formal sanction or expulsion, and in feud-code branches a lasting dishonor that can attach to kin. Their refusal is the code's enforcement target; most leave the niche entirely, which is what the sanction is for.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_refusers, payer,
    powerless, biographical, constrained, regional).

% Families of men killed or maimed under the code. They inherit the loss and, in feud-code branches, an obligation to further settlement that can pass to children. They were never parties to the challenge certification that produced the loss and have no seat in its aftermath.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_casualty_kin, payer,
    powerless, generational, constrained, regional).

% Enforce criminal prohibitions on dueling and feud violence. Their enforcement intensity sets the terms under which the code survives, driving it underground and shaping its ritualized forms. They bear prosecution costs, tolerate selectively under elite pressure (officer corps, fraternity fencing), and face revival episodes when state capacity collapses.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Wives, mothers, and daughters bound by the code's consequences — family standing, widowhood, inherited feud obligation — with no seat in challenge certification, seconds' negotiation, or sanction decisions. The code's costs reach them through kinship while its forums never admit them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_community_women, excluded,
    powerless, generational, constrained, regional).

% Abolitionist movements, religious bodies, and reform legislators who contest the code's legitimacy. They won the criminal prohibitions that drive this story, yet inside the niche's own forums their objection is structurally inadmissible — the code does not adjudicate its own legitimacy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, anti_dueling_campaigners, excluded,
    organized, biographical, mobile, national).

% Researchers of honor cultures — the culture-of-honor tradition, fencing-fraternity scholarship, feud-code ethnography. They see the whole structure across niches, what the code settles and what it costs and who bears it, from no seat in any of them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_ethnographers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, honor_community_custodians).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Inside the niche, the code settles disputes the host state's law does not recognize as actionable — insults, standing slights, challenges to honor — through bounded, rule-governed ritual (seconds, terms, weapons, medical attendance in the fencing branches) that caps escalation where unbounded feud is the alternative; it also maintains the niche's membership boundary and internal standing order, since willingness to answer is the proof of membership.
% TRANSFER_FUNCTION: Moves physical risk, injury, death, and legal jeopardy onto individual juniors who answer challenges; moves standing upward as juniors purchase admission to the hierarchy with risk-bearing; moves dispute-settlement authority from state courts into the niche's internal forum; in feud branches, moves settlement obligations down the generations within kin groups.
% ABSENT_VOICES: Women bound by the code's consequences sit in no forum — not in challenge certification, not in the seconds' negotiation, not in sanction decisions. Kin of the killed inherit costs without having been parties. Refusers are heard only as sanction targets. Abolitionist campaigners, whose objection produced the criminal prohibitions, are inadmissible inside the code's own adjudication — the code does not entertain challenges to itself.
% DISAPPEARANCE_RATIONALE: The niches' arrangements depend on the code. Fencing fraternities would lose their scar-based membership proof and the standing economy built on it; feud-code communities would shift to either state adjudication or unbounded vendetta; the standing hierarchies would need a replacement currency. The host societies' mainstream would barely register the loss — the rearrangement is niche-internal, which is precisely this reading's claim about where the arrangement lives.
% FOUNDING_PROBLEM: Pre-administrative and weak-state societies offered no credible forum for reputation injury; honor communities built a self-help settlement code — bounded, ritualized dueling — to resolve standing disputes and deter insult without sliding into unbounded feud.
% FOUNDING_PROBLEM_CORROBORATION: The state legal record corroborates the founding problem's substantial solution from outside the beneficiary set: the legislators and courts that criminalized dueling premised suppression on the state forum's sufficiency for violence and, later, for defamation, and historians of the decline concur that the original settlement function is largely superseded. The custodian communities alone attest residual liveness — that peer standing inside the niche is not judicially remediable — and no adjudicator outside the dispute exists for that residual claim.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: the residual code compels risk-bearing (injury, death in feud branches, prosecution) through standing sanctions while its settlement function has atrophied toward the vestigial — cost per unit of delivered settlement rose across the interval as ritualization outpaced function and legal jeopardy matured. Suppression 0.62 is authored as a raw structural property, unscaled by power or scope: internal sanctioning re-concentrated as the niches shrank — in a small community a refusal sanction bites harder and alternative standing venues are fewer — though exit itself remains possible at identity cost, which keeps suppression below trapped-grade. Theater_ratio 0.62 measures the performative share of the settlement apparatus specifically: arranged fencing bouts fought for scars rather than grievances, fascist-era duels fought for political display, challenge ritual maintained as boundary proof. Identity and boundary maintenance is real function for this arrangement's persistence and is not counted as theater. Accessibility_collapse 0.45: inside the niche, alternatives are partly collapsed — host courts do not adjudicate honor injury and refusal carries sanction — but exit from the niche remains and bodily harm is legally actionable, so collapse is partial, far from natural-law grade. Resistance 0.6: a century and a half of state criminalization, abolitionist campaigning, and internal refusals. The measurement series share one grid (seven points, three metrics each); the trajectories are trend-with-episodes rather than full cycles — toleration episodes (officer corps, fascist Italy, fraternity-fencing toleration) and crackdowns oscillate around a rising extraction trend, with the Kanun's post-1990s revival as the salient revival episode.
 *
 * PERSPECTIVAL GAP:
 *   The custodian seat computes coordination-dominant: from inside the administration, the code is the niche's constitution — dispute channel, boundary proof, standing order — and its costs are the price of the community's existence. The junior and refuser seats compute extraction-dominant: risk and sanction flow onto those with the least standing and no forum. The state seat experiences the arrangement as enforcement burden and selective-toleration scandal; the excluded seats (women, casualty kin) bear costs through kinship with no seat anywhere. The engine computes these per-seat classifications from the structural data; this story does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: honor_community_custodians sit nearest the beneficiary end — they administer the code and their authority exists only in its operation, and identity-locked exit deepens the subsidy; established_standing_holders collect standing from others' risk-bearing under the same identity lock. Targets: junior_honor_adherents bear injury risk and legal jeopardy as the price of admission — their prospective standing gain keeps them off the full-target end but high; duel_refusers bear sanction with no offsetting benefit, near full-target while they remain; duel_casualty_kin bear inherited costs with no seat at all. The excluded seats (women, campaigners) are commentary-grade and drive no classification. state_legal_authorities is enforcement context, not a party to the constraint's transfer; no directionality is authored for it and none is needed — the structural derivation handles the parties from the beneficiary and victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no credible forum for reputation injury — is contested rather than dead: host-state defamation and criminal law cover the bodily and much of the reputational ground, but the custodian tier attests that peer standing is not judicially remediable, and the arrangement's identity function is independently live. Mandatrophy is therefore not declared resolved. The classification guards both mislabelings: a pure-extraction reading would miss the genuine niche coordination (bounded dispute channeling that caps feud escalation, membership-boundary maintenance); a pure-coordination reading would miss the compulsion (refusal sanctions, birth-bound feud obligation) and the asymmetric risk distribution that concentrates standing benefits in the custodian tier while risk concentrates in the junior tier. The inertial-persistence reading fails the cost-asymmetry test: the administrators bear real, rising costs to maintain the code (legal exposure, member injuries) and would lose their standing by fixing it — this is loved, actively maintained practice, not unloved residue, even though the settlement function it was built for is largely vestigial and its performative share is high.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the honor_settlement_legitimacy kernel — the drop reading. How would instantiating the contraction reading (framework transformation, no live option) or the composite reading (overdetermined decline with contraction edge) change the constraint''s structure?',
    'Adopting the contraction reading dissolves the referent arrangement: with no live niche option there are no compelled participants, no sanction structure, and extractiveness collapses toward historical residue. Adopting the composite reading redistributes the burden across mechanisms and shrinks the niche-persistence share. The disagreement is located in one structural element: whether the normative repertoire retains operable alternatives in niches after suppression.',
    'Contraction adoption eliminates this constraint''s victim set and enforcement structure (no live arrangement left to classify); composite adoption lowers the authored extractiveness and recasts the niches as one term in a multi-mechanism account rather than the arrangement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: which reading of the honor-settlement kernel is instantiated, and what sibling readings would change structurally.').

omega_variable(
    niche_census_ambiguity,
    'Which practices count as this reading''s niches, and does the authored extractiveness hold across them?',
    'Ethnographic and legal census of active honor-settlement practice: ritualized non-lethal forms (academic fencing with protective gear and medical supervision) versus lethal feud codes (Kanun blood obligation, Caucasus vendetta). The authored value aggregates both; a census weighting toward ritualized forms lowers it, toward lethal feud codes raises it.',
    'If the live niche set is mostly ritualized and consensual, the arrangement moves toward the coordination-dominant range; if lethal feud obligation dominates, it moves toward the pure-extraction boundary within the niche.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(niche_census_ambiguity, empirical, 'The niche set''s composition drives the extraction/coordination balance.').

omega_variable(
    compulsion_voluntariness,
    'Is residual participation compelled (standing sanctions, birth-bound feud obligation, fraternity standing pressure) or voluntary (self-selected identity communities)?',
    'Refusal rates and sanction severity inside the niches; exit interviews with former fraternity members and emigrants from feud regions; comparison of joiners'' prior honor-culture exposure.',
    'If participation is substantially voluntary, the arrangement drifts toward pure coordination among the willing and extractiveness falls; if compulsion is load-bearing, the hybrid reading holds and the feud branches must be watched against the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsion_voluntariness, empirical, 'Whether the niche code binds the unwilling or coordinates the willing.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (sanctions, legal jeopardy, birth-bound obligation, closed forums) or internalized (adherents experience refusal as unthinkable dishonor)?',
    'Post-exit suppression trajectory: if former members and emigrants from feud regions carry the compulsion with them — declining settlement, feeling bound across borders — the internalized share is substantial.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and persists after exit; structural remedies (decriminalization, forum provision) would under-treat it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized compulsion in the residual honor code.').

omega_variable(
    founding_forum_residual_liveness,
    'Is the founding problem dead — does host-state law now suffice for reputation injury — or does the residual liveness claim hold (peer standing not judicially remediable)?',
    'Comparative assessment of state remedies for honor injury (defamation law''s reach and its social efficacy inside honor communities) against the niches'' own settlement record; the state legal record already corroborates substantial solution from outside the beneficiary set.',
    'If dead, the arrangement is mandate-outlived and drifts toward inertial persistence as its custodian tier ages; if live in the niches'' own terms, a genuine coordination function persists and the hybrid classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_forum_residual_liveness, conceptual, 'Genealogy status of the founding problem: superseded forum or residual live function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 170).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(hono_tr_t60, observed).
narrative_ontology:measurement(hono_tr_t90, honor_settlement_legitimacy__drop_reading, theater_ratio, 90, 0.5).
narrative_ontology:measurement_basis(hono_tr_t90, observed).
narrative_ontology:measurement(hono_tr_t120, honor_settlement_legitimacy__drop_reading, theater_ratio, 120, 0.56).
narrative_ontology:measurement_basis(hono_tr_t120, observed).
narrative_ontology:measurement(hono_tr_t150, honor_settlement_legitimacy__drop_reading, theater_ratio, 150, 0.6).
narrative_ontology:measurement_basis(hono_tr_t150, observed).
narrative_ontology:measurement(hono_tr_t170, honor_settlement_legitimacy__drop_reading, theater_ratio, 170, 0.62).
narrative_ontology:measurement_basis(hono_tr_t170, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(hono_be_t60, observed).
narrative_ontology:measurement(hono_be_t90, honor_settlement_legitimacy__drop_reading, base_extractiveness, 90, 0.59).
narrative_ontology:measurement_basis(hono_be_t90, observed).
narrative_ontology:measurement(hono_be_t120, honor_settlement_legitimacy__drop_reading, base_extractiveness, 120, 0.61).
narrative_ontology:measurement_basis(hono_be_t120, observed).
narrative_ontology:measurement(hono_be_t150, honor_settlement_legitimacy__drop_reading, base_extractiveness, 150, 0.63).
narrative_ontology:measurement_basis(hono_be_t150, observed).
narrative_ontology:measurement(hono_be_t170, honor_settlement_legitimacy__drop_reading, base_extractiveness, 170, 0.64).
narrative_ontology:measurement_basis(hono_be_t170, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(hono_su_t60, observed).
narrative_ontology:measurement(hono_su_t90, honor_settlement_legitimacy__drop_reading, suppression_requirement, 90, 0.58).
narrative_ontology:measurement_basis(hono_su_t90, observed).
narrative_ontology:measurement(hono_su_t120, honor_settlement_legitimacy__drop_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement_basis(hono_su_t120, observed).
narrative_ontology:measurement(hono_su_t150, honor_settlement_legitimacy__drop_reading, suppression_requirement, 150, 0.61).
narrative_ontology:measurement_basis(hono_su_t150, observed).
narrative_ontology:measurement(hono_su_t170, honor_settlement_legitimacy__drop_reading, suppression_requirement, 170, 0.62).
narrative_ontology:measurement_basis(hono_su_t170, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the decline of dueling' covers three structurally distinct claims and decomposes into a three-story family: honor_settlement_legitimacy__contraction_reading (framework transformation made dueling cognitively unthinkable; a dead practice leaves no live arrangement to classify), honor_settlement_legitimacy__composite_reading (decline overdetermined by multiple reinforcing mechanisms with a contraction edge; extraction distributed across mechanisms), and this file, honor_settlement_legitimacy__drop_reading (niche persistence; extractiveness authored over a live, suppressed, compelled arrangement with its own victim set and enforcement structure). Each member links the others here via affects_constraints. The downstream structure runs from the macro-decline readings to this residual claim: the drop reading's niche evidence is the data the other two must accommodate, and it pressures the composite reading's contraction-edge weighting without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
