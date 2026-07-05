% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Dueling as Honor-Vindication Norm — Composite (External-Cost + Conceptual-Contraction) Decline Reading
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the composite reading of the
 *   honor_violence_legitimacy kernel: dueling's decline as elite
 *   dispute-resolution practice is explained by two mechanisms operating
 *   simultaneously and interactively rather than either alone. The drop
 *   mechanism (rising legal, financial, and reputational cost of dueling
 *   imposed by state prosecution and social sanction) made the practice
 *   practically riskier without touching its underlying legitimacy claim. The
 *   contraction mechanism (moral and religious redefinition of honor to
 *   exclude violence and privilege self-restraint) made abstention from a
 *   challenge socially survivable for the first time, without needing
 *   external cost to do the work. The composite reading's structural claim is
 *   that the drop mechanism alone is insufficient to explain the pattern of
 *   decline — men with means to absorb the rising legal cost nonetheless
 *   stopped dueling at rates matching men without such means, once the
 *   contracted definition of honor gave them a legitimate off-ramp.
 *   Conversely, the contraction mechanism alone cannot explain why decline
 *   accelerated sharply in periods of most aggressive prosecution even among
 *   communities where the restrained-honor discourse had not yet taken hold.
 *   The two mechanisms have different victim sets: the drop mechanism
 *   disproportionately burdens men without wealth or patronage to absorb
 *   legal risk (challenged_men_of_modest_means, families_of_duelists); the
 *   contraction mechanism disproportionately burdens men in social networks
 *   slow to adopt the new vocabulary, who are branded cowards for declining
 *   before restraint became normatively legitimate in their circle. This is a
 *   genuinely distinct constraint from either sibling reading — its ε and
 *   beneficiary/victim structure emerge from the INTERACTION of the two
 *   mechanisms, not from either projected alone.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.52).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.58).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Dueling as Honor-Vindication Norm — Composite (External-Cost + Conceptual-Contraction) Decline Reading").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '027dad4b-ced1-44dd-b889-db83177f31db').
narrative_ontology:cs_kernel_codification('027dad4b-ced1-44dd-b889-db83177f31db', distributed).
narrative_ontology:cs_authority_grounding('027dad4b-ced1-44dd-b889-db83177f31db', practice).
narrative_ontology:cs_interpretation_layer_present('027dad4b-ced1-44dd-b889-db83177f31db').
narrative_ontology:cs_reading_relation('027dad4b-ced1-44dd-b889-db83177f31db', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('027dad4b-ced1-44dd-b889-db83177f31db', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('027dad4b-ced1-44dd-b889-db83177f31db', foundational, decline_requires_joint_mechanism_sufficiency).
narrative_ontology:cs_axiom_status(decline_requires_joint_mechanism_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('027dad4b-ced1-44dd-b889-db83177f31db', decline_requires_joint_mechanism_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('027dad4b-ced1-44dd-b889-db83177f31db', secondary, single_mechanism_accounts_are_individually_insufficient).
narrative_ontology:cs_axiom_status(single_mechanism_accounts_are_individually_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('027dad4b-ced1-44dd-b889-db83177f31db', single_mechanism_accounts_are_individually_insufficient, empirically_contingent).
narrative_ontology:cs_reference_frame('027dad4b-ced1-44dd-b889-db83177f31db', unmediated_status_equal_combat_adjudication).
narrative_ontology:cs_drift_state('027dad4b-ced1-44dd-b889-db83177f31db', post_dual_delegitimation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('027dad4b-ced1-44dd-b889-db83177f31db', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, dueling_class_incumbents).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, honor_code_arbiters).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, challenged_men_of_modest_means).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, families_of_duelists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_and_dependents_of_duelists).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, gentlemanly_status_requires_personal_vindication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established gentlemen who already hold reputational capital use the dueling code to police who counts as a peer and to settle disputes on terms that favor established skill, seconds, and social networks. As the practice declines, they retain the deference the code produced without needing to keep fighting for it — the reputational capital, once won, persists after the enforcement mechanism weakens.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_class_incumbents, beneficiary,
    powerful, biographical, constrained, national).

% Seconds, codes-of-honor authors, and dueling societies administer the rules that decide when an affront requires a challenge, what satisfies it short of bloodshed, and who may legitimately decline. They author both the escalating legal/social costs (drop mechanism) and the redefinition of gentlemanly conduct as self-restraint (contraction mechanism) — often the same men doing both, since arbitrating decline is itself a status-conferring role.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_code_arbiters, agenda_setter,
    organized, generational, mobile, national).

% Men without independent wealth or strong patronage networks face a challenge and cannot decline without social death, yet cannot afford the legal exposure (prosecution risk, forfeiture, exile) that rising external costs impose, nor can they claim the emerging vocabulary of restrained honor without appearing to concede cowardice to peers who have not yet accepted the redefinition. They are caught between two decaying legitimations moving at different speeds.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, challenged_men_of_modest_means, payer,
    moderate, immediate, trapped, national).

% Bear the material consequences when a duel ends in death, injury, prosecution, or exile of the breadwinner — loss of income, social stigma, legal liability for seconds and accomplices. They have no standing in the code's deliberations and no voice in either the legal reforms driving up cost or the moral redefinition changing what honor requires.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, families_of_duelists, payer,
    powerless, biographical, trapped, regional).

% Wives, mothers, and children absorb the risk of widowhood, disgrace, and loss of protector without ever participating in the honor code that generates the risk. Their objections, where recorded at all, appear in private letters and petitions rather than in the dueling literature itself — a voice present in the archive but absent from the deliberative process.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_and_dependents_of_duelists, excluded,
    powerless, biographical, trapped, regional).

% Legislatures and courts raise the external cost of dueling — criminalization, prosecution of seconds, forfeiture of commissions and offices, public disgrace campaigns — operating on the drop mechanism independently of, but reinforcing, the parallel moral redefinition of honor as self-command. Their statutes and prosecutions are documentary evidence for the drop edge of the composite reading.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, state_legal_authorities, observer).

% Clergy, essayists, and civic moralists actively campaign to redefine gentlemanly honor as restraint, sobriety, and reputation-through-virtue rather than reputation-through-combat, supplying the vocabulary that lets men decline challenges without automatic loss of standing. Their tracts and sermons are the documentary evidence for the contraction edge.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, moral_and_religious_reformers, agenda_setter,
    organized, generational, mobile, national).

% Reconstruct the decline from court records, code-duello literature, prosecution rates, and sermon archives, and adjudicate whether either single-mechanism account is sufficient or whether the two mechanisms operated jointly and interactively.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The dueling code, in its operative period, coordinated dispute resolution among status-equals without recourse to courts they considered beneath their dignity or courts that could not adequately vindicate reputational harm — it settled who was owed satisfaction and by what ritualized means, averting unbounded private violence.
% TRANSFER_FUNCTION: Moves reputational capital, social deference, and physical risk from the party judged to owe satisfaction to the party owed it; as decline sets in, it additionally transfers legal and financial risk (prosecution, forfeiture) onto participants who can least absorb it, since wealthier incumbents can better weather both the legal exposure and the social cost of publicly adopting the new restrained-honor vocabulary.
% ABSENT_VOICES: Women and dependents of duelists, and men from communities with no established second-and-arbiter network, are structurally outside the deliberations that produced both the escalating legal costs and the redefinition of honor — they experience the consequences of both mechanisms without input into either.
% DISAPPEARANCE_RATIONALE: The composite account holds that dueling's legitimacy did not collapse from a single shock but was hollowed out on two fronts at once: rising external cost made the practice practically riskier while the redefinition of honor made abstention socially survivable. Remove either mechanism and decline still occurs but on a different timeline and with a different residual population still dueling; remove both and the practice's social infrastructure (seconds, codes, dueling grounds, honor societies) would have persisted largely intact into the twentieth century, materially changing elite dispute-resolution norms that instead reorganized around courts, journalism, and civil-restraint codes.
% FOUNDING_PROBLEM: Among status-equals, no third party (court, church, crown) commanded sufficient shared legitimacy to adjudicate reputational injury, so a private ritualized combat procedure emerged to settle affronts to honor without the endless escalation of unregulated vendetta.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and court-record archivists (state legal authorities' own statutes and prosecution records) attest that by the mid-to-late period courts had become the accepted forum for reputational and defamation disputes among the same social class that had formerly dueled — corroboration from outside the honor-code arbiters themselves, who continued to defend the practice's residual legitimacy well after courts had displaced its founding function.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and theater_ratio (0.44) are authored higher than either single-mechanism reading would independently predict, because the composite reading captures a longer tail: the practice persists in attenuated, increasingly performative form (dueling as symbolic gesture, quickly interrupted by seconds, rarely lethal) precisely because neither mechanism alone was sufficient to fully delegitimize it — each covered for the other's gaps. Theater ratio rises steadily across the interval as the surviving dueling culture becomes progressively more ritual-without-teeth: fewer actual exchanges of fire, more staged confrontations resolved by apology, while the code-of-honor literature and societies persist administratively long after the practical function (settling disputes courts cannot reach) has been displaced by the very courts the drop mechanism strengthened. Suppression_requirement rises in step with theater_ratio because maintaining the residual code against two simultaneously eroding legitimations (external cost AND moral standing) requires increasing active work from the arbiter class — hence the piton reading: this is a constraint being kept alive by inertia and institutional performance from a shrinking administering class, not a currently-functioning coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the honor_code_arbiters' seat the composite pressures look like two separable inconveniences to be managed (litigate around the legal risk, rhetorically reconcile restraint with honor) rather than a joint delegitimation. From the challenged_men_of_modest_means seat, the two mechanisms compound: neither cost-avoidance nor honor-redefinition is individually available to them at the moment they need an exit, because their social circle has not yet absorbed the new vocabulary and they cannot afford the legal exposure the wealthy can. The engine's per-seat computation should register this asymmetry directly from the declared power/exit differentials.
 *
 * DIRECTIONALITY LOGIC:
 *   dueling_class_incumbents and honor_code_arbiters sit near the beneficiary end: their accumulated reputational capital and status as interpreters of the code survive its decline, and the deference the code produced does not evaporate merely because the code weakens. challenged_men_of_modest_means, families_of_duelists, and women_and_dependents_of_duelists sit near the target end: they bear the compounding risk of both mechanisms without commensurate voice or exit. state_legal_authorities and moral_and_religious_reformers are agenda-setters external to the honor economy itself — they act on it from outside rather than extracting from it, hence their power/exit profile (institutional/organized, mobile/analytical) reflects agents driving the mechanisms rather than being subject to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (adjudicating reputational injury among status-equals absent a trusted third-party forum) is dead by the mid-interval: courts have assumed that function. The code's continued operation past that point — increasingly theatrical, increasingly reliant on active social enforcement by a shrinking arbiter class to remain socially binding — is the signature this composite reading is built to detect: an overdetermined decline where two independent delegitimation pressures jointly hollow out a constraint's founding function while its administrative and performative shell persists. Classifying this purely under the drop or contraction reading alone would risk crediting one mechanism with a decline that required both; the composite reading exists precisely to prevent that misattribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_of_composite_over_single_mechanism,
    'Does the historical record actually require BOTH mechanisms jointly, or could a sufficiently fine-grained drop_reading (accounting for uneven prosecution intensity across regions) or a sufficiently fine-grained contraction_reading (accounting for uneven vocabulary adoption across social networks) explain the same uneven decline pattern alone?',
    'Comparative regional analysis cross-tabulating prosecution rates, sermon/tract circulation data, and dueling incident rates by decade and locality; if decline rate correlates with only one variable after controlling for the other, the composite reading collapses into its dominant sibling.',
    'If either single mechanism proves statistically sufficient once properly specified, this composite constraint should be retired in favor of the sufficient sibling reading — the composite reading is only warranted if genuine interaction (not mere co-occurrence) is empirically demonstrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_of_composite_over_single_mechanism, empirical, 'Whether joint mechanism specification is empirically necessary or whether one sibling reading, refined, is sufficient.').

omega_variable(
    victim_set_overlap_ambiguity,
    'To what extent do the victim sets of the drop mechanism (financially exposed challenged men) and the contraction mechanism (socially exposed men in slow-adopting networks) actually overlap in the same individuals, versus representing genuinely distinct populations?',
    'Prosopographical study of dueling participants in the decline period cross-referencing wealth records with social-network vocabulary adoption (correspondence, club membership, sermon attendance).',
    'High overlap would suggest the two mechanisms compound on the same vulnerable population (supporting a stronger composite/interaction reading); low overlap would suggest two largely separate populations each hit by one mechanism (weakening the case for genuine interaction over mere co-occurrence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_overlap_ambiguity, empirical, 'Whether the two mechanisms'' victims are the same people or different people.').

omega_variable(
    arbiter_class_self_interest_in_composite_framing,
    'Do honor_code_arbiters have an interest in narrating the decline as overdetermined (composite) rather than as capitulation to external legal pressure (which would concede defeat) or as their own moral conversion (which would concede error)? Does the composite framing itself serve a face-saving function for the class administering the code''s final decades?',
    'Compare arbiter-class self-narration in memoirs and dueling-society records against the independent legal and homiletic record for divergence in emphasis or chronology.',
    'If arbiter self-narration systematically overstates the contraction (moral) component relative to the documentary record, that overstatement is itself evidence of the piton dynamic — theatrical maintenance of a self-flattering decline narrative alongside the actually-declining practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbiter_class_self_interest_in_composite_framing, conceptual, 'Whether the composite framing is analytically warranted or partly a face-saving narrative produced by the class administering the code''s decline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(hono_tr_t50, honor_violence_legitimacy__composite_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(hono_tr_t60, honor_violence_legitimacy__composite_reading, theater_ratio, 60, 0.44).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(hono_be_t50, honor_violence_legitimacy__composite_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(hono_be_t60, honor_violence_legitimacy__composite_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(hono_su_t50, honor_violence_legitimacy__composite_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(hono_su_t60, honor_violence_legitimacy__composite_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This story is the composite member of a three-story kernel family (honor_violence_legitimacy). drop_reading claims decline is fully explained by rising external cost with legitimacy intact; contraction_reading claims decline is fully explained by moral redefinition rendering violence unthinkable independent of cost. composite_reading (this file) claims neither is independently sufficient and authors its own ε (0.52) reflecting an interaction effect distinct from either sibling's projected value — it does not average the siblings' metrics. The reading_relations are authored as 'influences' rather than 'forecloses' or 'coexists_with' because the composite reading's empirical claim (joint necessity) creates evidentiary pressure on each single-mechanism sibling without logically foreclosing them — a drop-only or contraction-only account remains logically coherent as a position, but the composite reading's success would undercut the sufficiency claim each sibling makes alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
