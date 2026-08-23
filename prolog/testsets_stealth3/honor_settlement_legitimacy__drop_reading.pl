% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Residual Honor Settlement Jurisdiction (Dueling Under Prohibition)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   Between roughly 1780 and 1940 the regulated duel passed from the center
 *   of gentlemanly dispute settlement to outlawed fringe practice — yet never
 *   to extinction: recorded encounters continued in military academies,
 *   planter enclaves, and clan districts, running to celebrated late affairs
 *   fought with edged weapons well into the twentieth century. This story
 *   models the standing arrangement the drop_reading of the
 *   honor_settlement_legitimacy kernel is about: a residual, actively
 *   enforced settlement jurisdiction in which affairs of honor remain
 *   answerable by regulated combat under formal prohibition. Its
 *   extractiveness is authored for THAT arrangement as the drop reading sees
 *   it — a live, costly, partly coerced mini-jurisdiction — not for the
 *   metropolitan order that abolished dueling and not for any arrangement a
 *   sibling reading would describe. Family note: the colloquial label 'the
 *   end of dueling' decomposes into three structurally distinct claims (this
 *   drop_reading; contraction_reading's cognitive closure;
 *   composite_reading's overdetermined closure with a contraction edge),
 *   linked through network.affects_constraints; each carries its own
 *   extractiveness over the shared referent, and this file averages over none
 *   of them.
 *
 * KEY AGENTS:
 *   - honor_code_custodians: agenda-setting administrator (powerful/identity_locked) — adjudicates affairs of honor, interprets and transmits the code, sanctions refusals; the office exists only while the settlement commands assent
 *   - established_honor_elites: primary beneficiary (powerful/mobile) — collects credible status protection from the code's operation while shifting frontline exposure downward
 *   - junior_officers_challenged: primary target (moderate/trapped) — answers challenges under sanction of social death; bears the arrangement's mortal and legal costs
 *   - duel_casualties_kin: uncompensated bearer (powerless/trapped) — inherits death, injury, and legal stigma with no seat in the settlement's terms
 *   - state_prohibition_authorities: opposing administrator (institutional/constrained) — prosecutes duels under statutes it cannot fully enforce; its selective enforcement fixes the practice's fringe form
 *   - anti_dueling_reform_societies: excluded advocate (organized/mobile) — campaigns outside the niche's internal settlement
 *   - clergy_moral_authorities: excluded conscience (organized/mobile) — condemns the practice where it cannot veto it
 *   - niche_volunteer_duelists: committed continuer (moderate/identity_locked) — the residual adherents whose documented practice constitutes this reading's evidence; dual-positioned as bearer and receiver
 *   - historical_analyst_seat: analytical observer (analytical/analytical) — reconstructs the full structure from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.62).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.55).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Residual Honor Settlement Jurisdiction (Dueling Under Prohibition)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'ec755c1d-09b9-47c2-afc6-c33ffd5f97ae').
narrative_ontology:cs_kernel_codification('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', formalized).
narrative_ontology:cs_authority_grounding('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', lineage).
narrative_ontology:cs_interpretation_layer_present('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae').
narrative_ontology:cs_reading_relation('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', honor_settlement_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', foundational, normative_repertoire_persistence).
narrative_ontology:cs_axiom_status(normative_repertoire_persistence, holdable).
narrative_ontology:cs_axiom_grounding('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', normative_repertoire_persistence, empirically_contingent).
narrative_ontology:cs_axiom('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', secondary, suppression_scoped_not_transformative_decline).
narrative_ontology:cs_axiom_status(suppression_scoped_not_transformative_decline, holdable).
narrative_ontology:cs_axiom_grounding('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', suppression_scoped_not_transformative_decline, empirically_contingent).
narrative_ontology:cs_reference_frame('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', chivalric_dispute_settlement_continuity).
narrative_ontology:cs_drift_state('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', post_prohibition_fringe_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ec755c1d-09b9-47c2-afc6-c33ffd5f97ae', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_code_custodians).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, established_honor_elites).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, junior_officers_challenged).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, duel_casualties_kin).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, niche_volunteer_duelists).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, niche_volunteer_duelists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The men who keep the code: authors and interpreters of dueling regulations, presidents of courts of honor, senior seconds. They decide what counts as an affront, what satisfaction suffices, and what refusal costs. Their office, precedence, and social weight exist only while the settlement they administer is treated as binding; stepping outside it would dissolve the authority they hold.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_code_custodians, agenda_setter,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, honor_code_custodians, beneficiary).

% Senior officers, clan heads, and established gentry whose standing rests on the code's credibility. They rarely stand on the field themselves anymore; the assurance that insults will be answered is collected across the whole rank, while its price is paid overwhelmingly by its youngest members. Retirement to country estates or capital society thins their exposure without requiring them to renounce the code.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, established_honor_elites, beneficiary,
    powerful, biographical, mobile, continental).

% Subalterns and younger sons who must meet a challenge or wear the coward's mark; regimental advancement and marriage prospects hang on the answer. Apology protocols, courts, and flight exist on paper, but inside the enclave each ends a career and a name. They bear the encounters' wounds, the prosecutions, and the discipline, disproportionately relative to their years of exposure.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, junior_officers_challenged, payer,
    moderate, immediate, trapped, national).

% Widows, parents, and children who inherit the outcomes: deaths prosecuted as manslaughter, disabling injuries, and the legal stain attaching to the household. They surface in the record as witnesses and petitioners and had no part in setting the terms they live under; nothing in the settlement compensates them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, duel_casualties_kin, payer,
    powerless, generational, trapped, local).

% Legislatures, courts-martial, and magistrates carrying statutes against dueling. Enforcement depends on willing witnesses and unsympathetic juries, so it lands unevenly, mostly on juniors and the unlucky. The practice's stubborn survival keeps dockets, inquests, and disciplinary machinery occupied; outright toleration, meanwhile, invites accusations of class favoritism from the reform press.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_prohibition_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Voluntary associations, pamphleteers, and parliamentarians campaigning to criminalize and stigmatize the duel. They win statutes and platform condemnations but hold no seat where affairs of honor are actually negotiated; their appeals tend to bounce off the enclaves, where outside opinion is taken as confirmation that the code still matters.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, anti_dueling_reform_societies, excluded,
    organized, biographical, mobile, continental).

% Denominational bodies and preachers condemning the duel as sin and suicide. Their censures are heard, recorded, and disregarded whenever an affair of honor is actually pending; within the enclaves they counsel families after burials rather than principals before dawn appointments.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, clergy_moral_authorities, excluded,
    organized, biographical, mobile, continental).

% Officers and gentlemen in the enclaves who continue to send and accept challenges long after prohibition — clan districts, cadet cultures, planter circles — treating the settled encounter as the only currency their standing accepts. Some fight eagerly; others are carried along by the same expectations they help enforce on juniors. Leaving the frame would mean ceasing to be what they understand themselves to be.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, niche_volunteer_duelists, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, niche_volunteer_duelists, beneficiary).

% Historians and historical sociologists reconstructing the practice from trial records, correspondence, regimental archives, and the press; positioned outside every faction, able to set the enclaves against the metropolitan mainstream and compare readings of the same record.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, historical_analyst_seat, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, honor_code_custodians).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within residual honor communities, the duel provided a terminal, rule-governed settlement for disputes of personal insult among status equals: seconds negotiated terms, published codes fixed weapons and conditions, and a single bounded encounter stood in place of open-ended retaliation, ambush, or feud.
% TRANSFER_FUNCTION: Moves mortal risk and criminal liability from the community collectively onto individual principals — disproportionately junior men — while moving assured status protection to every member (each knows an unanswered insult damages the insulter) and moving deference and adjudication authority upward to the code's custodians.
% ABSENT_VOICES: The duel's dead, their widows and children, women generally, the clergy, and the non-honor professions had no seat in the councils where affairs of honor were framed; inside the niches, a member who disputed the practice's premise had already forfeited standing, so internal dissent was structurally self-silencing. Public abolitionists addressed legislatures and newspapers, not the negotiating table where terms were set.
% DISAPPEARANCE_RATIONALE: Overnight removal would force the residual niches to improvise dispute settlement — litigation, ostracism, or covert violence — dissolve the custodial offices whose authority rests on adjudicating the code, and reprice the status economies of the affected regiments and gentry circles. Metropolitan society would register little; the enclave orders would visibly reorganize around whatever settlement they adopted next.
% FOUNDING_PROBLEM: In stations where central courts offered no adequate remedy for insult among equals, and where ignoring an insult invited endless retaliation, honor societies needed a bounded, mutually recognized terminal settlement; the regulated duel arose as the substitute for private war and vendetta.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting circle, coroners' inquests, courts-martial records, parliamentary committee proceedings (including the nineteenth-century British revisions that struck dueling articles from the Articles of War), and denominational condemnations attest that civil and military law had absorbed the insult-remedy function across most jurisdictions by the mid-1800s; meanwhile the enclaves' own correspondence and defense-counsel arguments attest a perceived continuing need inside them. Both witness strata sit outside the custodial beneficiary set, and they disagree — hence contested rather than dead.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   All series run on one shared grid, T=0..60 in decade steps (mapping approximately 1780–1940); every tracked metric is authored at every examined point. Extractiveness climbs from 0.48 to 0.62: as the practice contracted, its burdens concentrated — the remaining principals stacked mortal risk atop new criminal liability, and the ratio of exposed juniors to shielded elders worsened inside the shrinking pool. Theater rises from 0.14 to 0.50 as prohibition pushed encounters toward negotiated satisfaction, bloodless affairs, and ceremony — proxy performance substituting for settlement — while the enclave core kept the lethal form alive; that substitution is Goodhart-style drift visible in the series. The suppression series falls from 0.72 to 0.55, modeling erosion of the code's coercive machinery as metropolitan honor culture dissolved: broad social compulsion gave way to identity-bound compulsion in the enclaves, where the grip stabilized rather than vanishing. The gradual accumulation of extractiveness over the interval is the pattern abductive drift-checks watch for; it is reported as a hypothesis for investigation, not tuned toward any threshold.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. The custodian experiences stewardship of a trust that constitutes his authority; the junior experiences a mortal levy he cannot refuse without social execution; the state experiences an ungovernable residue consuming prosecution capacity it cannot politically withdraw; the kin experience bereavement administered by other people's honor; the volunteer experiences freedom exercised under his own code. Nothing in the authored claim adjudicates among these — the divergence follows from the structural data, and measuring it is the point of the corpus.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the custodians and the established elites at the low-d end of the scale; victim declarations place the challenged juniors and the casualties' kin at the high-d end. The state appears on neither list, so its canonical fallback would misrepresent it: it bears net enforcement costs that the arrangement's persistence imposes, so a directionality override sets the institutional atom near the target end (0.8). The volunteer duelists share the juniors' power atom but not their position — they are dual-seated (bearer and receiver) and identity-locked; the per-atom override mechanism cannot distinguish them, so their divergence is carried by the dual-role declaration and flagged in the omegas rather than papered over with an override the format cannot express precisely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a terminal, recognized remedy for insult among equals where the courts gave none — is dead across the metropolitan mainstream and attested live only inside the enclaves: the mandate has outlived its function across most of its former range while retaining it in pockets. Reading the residue as pure predation erases the settlement service the adherents themselves attest and would misread the volunteers' own testimony; reading it as healthy coordination erases the juniors' coerced exposure and the kin's uncompensated losses. The hybrid classification holds both facts simultaneously, which is why the claim is authored as tangled_rope rather than collapsed either way. The theater trajectory marks the drift path ahead: if the niches hollow further, what remains is ceremony administered by offices whose function has gone — the signature the lifecycle detectors watch for, already latent in the rising performative share.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of the honor_settlement_legitimacy kernel — what would change structurally if a sibling reading were instantiated instead?',
    'Author the sibling stories separately and compare computed classifications across the family: contraction_reading relocates the operative limit inside cognition and predicts zero live practice once frameworks shift; composite_reading attributes the residue to the thinning tail of several reinforcing mechanisms rather than to durable niche authority.',
    'The standing arrangement''s classification flips with the reading: under contraction_reading the residue is inert remainder; under this drop_reading it is a functioning, enforced mini-jurisdiction; under composite_reading its weight shifts toward whichever tail mechanism dominates. The disagreement is located at one structural element: whether the normative repertoire''s closure over dueling was total-and-cognitive or partial-and-enforcement-scoped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame locator: which kernel reading this story instantiates, what siblings would alter, and where the disagreement sits.').

omega_variable(
    voluntary_vs_coerced_persistence,
    'Among the residual adherents who fought duels after prohibition, how many participated voluntarily and how many under compulsion of the code?',
    'Micro-historical analysis: trial depositions, seconds'' memoirs, and correspondence distinguishing solicited affairs from demanded ones across the niches (cadet corps, planter enclaves, clan districts).',
    'If compulsion dominates, the arrangement sits nearer pure predation and the junior payer seat''s position hardens; if voluntariness dominates, the settlement-service reading strengthens and measured burden on participants falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_coerced_persistence, empirical, 'Whether fringe persistence reflects willing adherence or enforced conformity.').

omega_variable(
    suppression_mechanism_split,
    'Is the compulsion that binds niche adherents to answer challenges structural (regimental gates, community sanction, marriage market exclusion) or internalized (honor fused with self-concept such that refusal is unthinkable even absent sanctions)?',
    'Post-exit trajectories: track men who left honor communities entirely; if the felt obligation persisted after sanctions could no longer reach them, the internalized share is large.',
    'An internalized share raises the arrangement''s effective grip above what structural sanctions alone predict and forecasts persistence even under full enforcement relief; a purely structural reading predicts rapid collapse once enforcement slackens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized compulsion in the residual honor communities.').

omega_variable(
    enforcement_elasticity_counterfactual,
    'Did dueling persist because demand for honor settlement persisted among adherents, or because suppression was selectively under-applied by sympathetic elites and nullifying juries?',
    'Compare niches with comparable adherent demand but differing enforcement intensity, and align prosecution waves with subsequent incidence: if incidence tracks enforcement effort rather than prior demand, supply-side tolerance explains the residue.',
    'Demand-side persistence supports this reading against contraction_reading; pure enforcement-tolerance would recast the residue as tolerated rather than resistant practice and lower the arrangement''s independent vitality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_elasticity_counterfactual, empirical, 'Whether the fringe survived on demand-side commitment or supply-side leniency.').

omega_variable(
    residual_scope_magnitude,
    'How large were the residual niches — is fringe persistence a rounding error among eligible men, or regionally substantial practice?',
    'Incidence series from coroners'' inquests, courts-martial records, and press reports normalized against eligible male populations, disaggregated by region and corps.',
    'If negligible, the drop reading survives only as a footnote and the arrangement''s aggregate weight collapses toward inertia; if regionally substantial, the niche jurisdiction carries real classification weight and the persistence claim is load-bearing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(residual_scope_magnitude, empirical, 'Magnitude of the geographic and social enclaves sustaining the practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__drop_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t10, honor_settlement_legitimacy__drop_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t20, honor_settlement_legitimacy__drop_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__drop_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t40, honor_settlement_legitimacy__drop_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement_basis(hono_tr_t40, observed).
narrative_ontology:measurement(hono_tr_t50, honor_settlement_legitimacy__drop_reading, theater_ratio, 50, 0.47).
narrative_ontology:measurement_basis(hono_tr_t50, observed).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__drop_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(hono_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__drop_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t10, honor_settlement_legitimacy__drop_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t20, honor_settlement_legitimacy__drop_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__drop_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t40, honor_settlement_legitimacy__drop_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(hono_be_t40, observed).
narrative_ontology:measurement(hono_be_t50, honor_settlement_legitimacy__drop_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(hono_be_t50, observed).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__drop_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(hono_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__drop_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t10, honor_settlement_legitimacy__drop_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t20, honor_settlement_legitimacy__drop_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__drop_reading, suppression_requirement, 30, 0.59).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t40, honor_settlement_legitimacy__drop_reading, suppression_requirement, 40, 0.57).
narrative_ontology:measurement_basis(hono_su_t40, observed).
narrative_ontology:measurement(hono_su_t50, honor_settlement_legitimacy__drop_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement_basis(hono_su_t50, observed).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__drop_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement_basis(hono_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial notion 'the end of dueling' splits into three structurally distinct claims sharing one referent arrangement (the honor settlement regime and its suppression). This drop_reading authors the enforced-but-partial closure: the repertoire stays live in niches and the binding limit is external suppression. contraction_reading authors cognitive closure: the practice ended because frameworks transformed, making the residue cognitively unavailable. composite_reading authors overdetermination: multiple reinforcing mechanisms with a contraction-weighted edge. Each file carries its own extractiveness over the same referent; the upstream metropolitan-core account (contraction) and this peripheral-residue account exert mutual evidential pressure, registered through the family links rather than merged into one story with a variable conclusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__drop_reading, institutional, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
