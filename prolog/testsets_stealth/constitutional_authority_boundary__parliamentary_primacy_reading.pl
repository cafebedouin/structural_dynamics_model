% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority
 *   domain: constitutional law/political philosophy/institutional design
 *
 * SUMMARY:
 *   In polities that instantiate this reading, the constitutional text —
 *   written or unwritten — does not bind the elected legislature. Parliament
 *   defines constitutional meaning by ordinary statute whenever it chooses,
 *   and may entrench instruments when it judges stability worth the cost, but
 *   no entrenchment survives a determined successor majority and no court may
 *   set primary legislation aside. Constitutional adjudication therefore runs
 *   downstream of legislative choice: courts apply, construe, and police
 *   administration, while the power to say what the constitution requires
 *   rests with whichever faction commands the chamber. The arrangement is
 *   authored here as a coordination mechanism that fixes a single,
 *   electorally accountable decision point for constitutional questions; the
 *   metrics describe its actual operation as assessed from this reading's own
 *   lights — low extraction, modest suppression, low theatrical content.
 *   Claim and metrics are independent authored facts: the engine computes
 *   per-seat classifications from the structural data, and divergence between
 *   the authored rope claim and any computed seat-level type is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - governing_parliamentary_majority: agenda-setter and primary beneficiary (institutional/arbitrage) — defines constitutional meaning and can restructure the arrangement at will
 *   - national_electorates: beneficiary (organized/constrained) — popular sovereignty located in the electoral loop that installs and removes majorities
 *   - appellate_judiciary: bearer (institutional/identity_locked) — applies statutes it cannot invalidate; professional role constituted inside the arrangement
 *   - opposition_parties: bearer with rotation prospect (powerful/constrained) — bears outcomes it opposes until it wins office
 *   - rights_minorities: bearer (powerless/trapped) — protection runs through majority goodwill; thinnest electoral leverage where stakes are highest
 *   - constitutional_scholars: analytical observer (analytical/analytical)
 *   - supranational_rights_bodies: excluded (institutional/trapped) — assert oversight with no domestic constitutional standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.3).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional law/political philosophy/institutional design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '6321f8d5-18c2-4fe6-b8d5-f638fdbc8597').
narrative_ontology:cs_kernel_codification('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', formalized).
narrative_ontology:cs_authority_grounding('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', practice).
narrative_ontology:cs_interpretation_layer_present('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597').
narrative_ontology:cs_reading_relation('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', foundational, elected_accountability_entitles_legislative_finality).
narrative_ontology:cs_axiom_status(elected_accountability_entitles_legislative_finality, holdable).
narrative_ontology:cs_axiom_grounding('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', elected_accountability_entitles_legislative_finality, deontological).
narrative_ontology:cs_axiom('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', foundational, no_text_binds_successor_parliaments).
narrative_ontology:cs_axiom_status(no_text_binds_successor_parliaments, holdable).
narrative_ontology:cs_axiom_grounding('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', no_text_binds_successor_parliaments, conventional).
narrative_ontology:cs_reference_frame('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', legislative_supremacy_framework).
narrative_ontology:cs_drift_state('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', contemporary_post_dialogue_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6321f8d5-18c2-4fe6-b8d5-f638fdbc8597', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, appellate_judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parties).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, rights_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commands the legislature and therefore defines constitutional meaning: drafts, passes, amends, or repeals whatever constitutional instruments it chooses, including entrenched ones, and no court can set its enactments aside. Collects final interpretive authority directly and can restructure the arrangement itself by ordinary statute. Its constraint on everyone else expires the day it loses an election.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities, beneficiary).

% Holds the ultimate check: installs and removes the majorities that define constitutional meaning, and the reading locates popular sovereignty in exactly this loop. Receives constitutional government filtered through electoral competition but possesses no direct instrument of constitutional decision between elections.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, national_electorates, beneficiary,
    organized, biographical, constrained, national).

% Applies legislation as enacted and polices the boundaries of administrative power, but must give effect to any statutory text however constitutional its subject: it cannot invalidate primary legislation and can signal disagreement only through interpretation, declarations, and extra-judicial speech. Its professional role as apolitical interpreter is constituted inside an arrangement it did not choose and cannot renounce without constitutional rupture.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, appellate_judiciary, payer,
    institutional, biographical, identity_locked, national).

% Contests elections under rules the governing majority can rewrite and bears constitutional outcomes it opposed; its protection runs through winning office, not through any forum where the arrangement itself can be challenged. Rotation prospects soften the position: today's bearer commands the arrangement after the next election.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, opposition_parties, payer,
    powerful, biographical, constrained, national).

% Depends for protection on the goodwill of shifting majorities: no court can strike down the statutes that burden it, and its recourse is persuasion, coalition-building, or exit from the jurisdiction. Electoral leverage is thinnest exactly where its stakes are highest.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, rights_minorities, payer,
    powerless, generational, trapped, national).

% Analyzes, criticizes, and theorizes the arrangement from outside its operation, comparatively across systems; influences doctrine at the margins through argument and appointment debates but holds no decision power over constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% Asserts human-rights oversight reaching into domestic law but holds no domestic constitutional standing under this reading: its judgments bind only through voluntary legislative incorporation, which the sovereign legislature can revoke at will.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, supranational_rights_bodies, excluded,
    institutional, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_majorities).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single, determinate decision point for constitutional questions: instead of unresolved inter-branch contests over fundamental law, one institution — the elected chamber — settles what the constitution requires, and every other actor adapts. Finality and accountability are supplied once, centrally, rather than renegotiated case-by-case between rival branches.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from any competing claimant (courts, prior parliaments, entrenched texts, external bodies) to the currently governing legislative majority; correspondingly moves the security of constitutional protections from courtrooms to electoral outcomes.
% ABSENT_VOICES: Those with the thinnest electoral leverage — disenfranchised residents, children and future generations, unpopular minorities — have no seat where constitutional meaning is fixed; they would object that electoral accountability is a promise of attention, not a protection. Supranational rights bodies stand wholly outside the domestic conversation. Their objections register only as persuasion addressed to the very majorities they need restraining.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, every contested statute would immediately reopen the question of who decides: courts would either claim invalidation power (migrating toward the judicially-supreme structure) or confess inability while another arbiter emerged, and the security of every statutory right would be repriced. Inter-branch relations, legislative drafting practice, and rights-advocacy strategy would all reorganize around whatever settlement replaced it.
% FOUNDING_PROBLEM: Inter-institutional deadlock over fundamental law: when crown, parliament, and courts each claimed to speak for the constitution, no determinate answer existed to the question of who decides, and constitutional disputes risked escalating into regime crises. The arrangement answers by locating final authority in the electorally accountable chamber.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional-history scholarship on the English seventeenth-century settlements and on written-constitution drafting debates attests the deadlock problem and its continuing salience; senior judges in parliamentary-sovereign systems repeatedly attest from the bench that they hold no invalidation power — an admission against interest from a bearing seat; and advocates of the sibling readings concede the deadlock problem is real while disputing this solution. No attesting source is confined to the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.20) because, assessed by this reading's own lights, the arrangement allocates rather than takes: the judiciary's loss of invalidation power is not a taking of anything the reading recognizes courts as entitled to, and the electorate receives constitutional government through the accountability loop it controls. The epsilon referent is the standing parliamentary-primacy arrangement itself, never a preferred alternative. Suppression (0.30) is a raw structural property, unscaled by power or scope: within the system, courts have no alternative but compliance and minorities have no alternative forum, yet the arrangement demands little active coercion because it is largely self-enforcing convention. Theater ratio is low (0.12): sovereignty rhetoric is ceremonial, but the mechanism genuinely settles constitutional questions. Accessibility collapse is moderate (0.40): understanding the arrangement does not close the alternatives — judicially-supreme and coordinate constructions remain live institutional choices operated elsewhere, and domestic actors periodically press for them. Resistance (0.38) reflects continuous scholarly and occasional judicial pushback without mass mobilization. Coordination type is enforcement_mechanism: the arrangement is a governance framework whose dedicated enforcement infrastructure is unusually light — chiefly the courts' self-application of the no-invalidation rule. The measurement series share one grid (t=0..30, years of a mature system); the gentle upward drift in base_extractiveness tracks majorities learning to exploit the arrangement's flexibility (executive dominance), and the rising suppression_requirement tracks the growing friction of holding judicial innovation inside interpretive bounds.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the governing majority's seat the arrangement is self-government: it obeys nothing it cannot remake and can restructure the rules that bind everyone else. From the appellate bench the same structure is subordination experienced daily — every statutory text must be given effect regardless of constitutional objection, and the bench's professional identity is fused with accepting that. From rights minorities it is exposure: the absence of a backstop is felt precisely when majorities are least sympathetic. The opposition seat oscillates with the electoral cycle — payer today, agenda-setter next term — a time-indexed role shift the static dial-set backgrounds (OQ-83) and which the engine reads through exit options rather than role labels.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: parliamentary_majorities (agenda-setter plus beneficiary, arbitrage exit) sit nearest the beneficiary pole; national_electorates (declared beneficiary, diffuse incidence) sit low-d but not at zero, since indirect costs reach them through policy. Appellate judiciary, opposition parties, and rights minorities are seated as payers with constrained or locked exits, placing them toward the target pole — the judiciary most firmly, since identity lock removes even hypothetical exit. No directionality overrides were authored: the beneficiary/victim-plus-exit derivation captures the structure, and the one candidate correction (opposition rotation toward symmetry) is handled in commentary because overrides key on power atoms that other seats share.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-branch deadlock over who decides fundamental law — remains live wherever the arrangement operates, so no mandatrophy declaration is authored and the arrangement is not maintained theatrically. The classification guards both directions: it prevents critics from mislabeling a functioning coordination settlement as pure extraction (the low authored epsilon records the reading's own assessment of the standing arrangement), and the temporal series plus the minority-protection omega prevent the reading's endorsement from laundering accumulating extraction — if electoral substitution fails descriptively, the rope tilts toward tangled_rope on the engine's computation rather than on this file's claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the constitutional_authority_boundary kernel — the parliamentary_primacy_reading. Would a polity governed by a sibling reading (judicial_supremacy_reading or coordinate_construction_reading) instantiate a structurally different constraint with a different beneficiary map?',
    'Observe adoption and abandonment events: entrenchment clauses creating judicial guardianship, court-curbing or court-expansion episodes, and written constitutions allocating final interpretive authority. Each event reveals which reading the polity operationalizes.',
    'Under judicial_supremacy_reading the judiciary becomes agenda_setter and beneficiary while parliamentary majorities become constrained payers, inverting this story''s directionality profile; under coordinate_construction_reading no seat captures final authority and gain_flow goes diffuse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: this story instantiates one reading of a contested kernel; siblings are separate constraints.').

omega_variable(
    final_authority_location_dispute,
    'Where exactly is the inter-reading disagreement located — on the existence of a final arbiter, on its identity, or on the remedy structure available when branches collide?',
    'Doctrinal analysis of collision cases: identify which institution''s determination prevails when legislative and judicial constitutional interpretations conflict, and whether any remedy (amendment, override, entrenchment) exists against that determination.',
    'Locating the dispute on the identity of the final arbiter supports the foreclosure edges authored in cs_structure; locating it instead on remedy intensity would downgrade the coordinate_construction edge to influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(final_authority_location_dispute, conceptual, 'Structural location of the disagreement between sibling readings.').

omega_variable(
    minority_protection_adequacy,
    'Do electoral channels adequately substitute for a judicial backstop in protecting political minorities under this reading, as the reading''s own lights assert?',
    'Comparative outcomes research: track rights-protective outcomes for unpopular minorities across parliamentary-sovereign and judicially-supreme systems, controlling for wealth and democratic age.',
    'If electoral channels systematically fail, extraction from rights_minorities rises, the rope classification tilts toward tangled_rope, and the low authored epsilon understates the standing arrangement''s burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_adequacy, empirical, 'Whether the reading''s substitution claim (elections replace judicial review) holds descriptively.').

omega_variable(
    entrenchment_mode_decomposition,
    'Does the reading''s tolerance of entrenched legislation remain the same constraint as ordinary-legislation primacy, or does heavy entrenchment migrate the structure toward a guardian-style arrangement warranting a separate story?',
    'Epsilon-invariance test: assess the constraint restricted to ordinary legislation versus entrenched instruments; if epsilon and the four positional atoms diverge across the two modes, decompose into two linked stories per DP-001.',
    'If the entrenchment mode diverges, a second constraint story (an entrenched-primacy variant) joins the network and this story''s epsilon narrows to the ordinary-legislation mode.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entrenchment_mode_decomposition, conceptual, 'Potential epsilon-invariance decomposition between ordinary and entrenched legislative modes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.07).
narrative_ontology:measurement(cons_tr_t6, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(cons_tr_t12, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(cons_tr_t18, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t6, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(cons_be_t12, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement(cons_be_t18, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 18, 0.18).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cons_su_t6, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement(cons_su_t12, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 12, 0.26).
narrative_ontology:measurement(cons_su_t18, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 18, 0.28).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional authority' covers three structurally distinct arrangements with different epsilon values and beneficiary maps: parliamentary primacy (this file — legislature collects final authority; low extraction assessed from the reading's own lights), judicial supremacy (courts collect; legislative majorities and popular majorities become constrained payers), and coordinate construction (no capturer; gains diffuse). Each is authored as its own epsilon-invariant story; family members link via affects_constraints so contamination analysis can trace, for example, how entrenchment practice under one reading pressures the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
