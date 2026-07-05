% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-primacy reading of the
 *   constitutional-authority-boundary kernel: the elected legislature, not
 *   the judiciary, holds final authority to fix constitutional meaning, and
 *   can do so through ordinary or entrenched legislation that survives
 *   judicial disagreement. This is a distinct constraint from the
 *   judicial-supremacy reading (where courts hold unchallengeable final say)
 *   and the coordinate-construction reading (where authority is distributed
 *   across co-equal branches with no final arbiter) — each reading has its
 *   own beneficiary structure, its own extraction profile, and its own
 *   failure mode, and none of them is evaluated here except by named
 *   cross-reference. Under this reading, extraction is low: the legislature's
 *   unbound interpretive authority is democratically self-correcting (a
 *   future majority can reverse a present one), and the primary cost falls on
 *   entrenched-rights claimants and courts whose institutional purpose is
 *   structurally narrowed. The claim (rope) and the metrics (low extraction,
 *   moderate accessibility collapse, moderate resistance) are authored
 *   independently and happen to sit close together here — this reading
 *   genuinely does look close to coordination-with-minor-cost from the
 *   structural data, unlike the eigenvector-thermalization-style contested
 *   cases.
 *
 * KEY AGENTS:
 *   - elected_legislature: agenda_setter (institutional/arbitrage) — sets and revises constitutional meaning via statute
 *   - electoral_majority_coalitions: beneficiary (organized/mobile) — holds unbound interpretive advantage while in power
 *   - constitutional_courts: payer/excluded (institutional/constrained) — review function narrowed to advisory or overridable
 *   - entrenched_minority_rights_claimants: payer (powerless/trapped) — protections vulnerable to majority reversal
 *   - future_legislatures: beneficiary/excluded (institutional/mobile) — inherit but do not consent to current exercise
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — compares outcomes across kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.3).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, 'a459d4dc-2108-4b9b-ab70-befbdcfd5053').
narrative_ontology:cs_kernel_codification('a459d4dc-2108-4b9b-ab70-befbdcfd5053', distributed).
narrative_ontology:cs_authority_grounding('a459d4dc-2108-4b9b-ab70-befbdcfd5053', practice).
narrative_ontology:cs_interpretation_layer_present('a459d4dc-2108-4b9b-ab70-befbdcfd5053').
narrative_ontology:cs_reading_relation('a459d4dc-2108-4b9b-ab70-befbdcfd5053', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('a459d4dc-2108-4b9b-ab70-befbdcfd5053', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('a459d4dc-2108-4b9b-ab70-befbdcfd5053', foundational, electoral_accountability_grounds_interpretive_finality).
narrative_ontology:cs_axiom_status(electoral_accountability_grounds_interpretive_finality, holdable).
narrative_ontology:cs_axiom_grounding('a459d4dc-2108-4b9b-ab70-befbdcfd5053', electoral_accountability_grounds_interpretive_finality, conventional).
narrative_ontology:cs_axiom('a459d4dc-2108-4b9b-ab70-befbdcfd5053', secondary, no_generation_may_permanently_bind_a_future_legislature).
narrative_ontology:cs_axiom_status(no_generation_may_permanently_bind_a_future_legislature, holdable).
narrative_ontology:cs_axiom_grounding('a459d4dc-2108-4b9b-ab70-befbdcfd5053', no_generation_may_permanently_bind_a_future_legislature, conventional).
narrative_ontology:cs_reference_frame('a459d4dc-2108-4b9b-ab70-befbdcfd5053', westminster_legislative_supremacy_framework).
narrative_ontology:cs_drift_state('a459d4dc-2108-4b9b-ab70-befbdcfd5053', contemporary_rights_jurisprudence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('a459d4dc-2108-4b9b-ab70-befbdcfd5053', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majority_coalitions).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, entrenched_minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, future_legislatures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning through ordinary or entrenched legislation, and can override or amend judicial interpretation through subsequent statute. Justifies this as the direct expression of democratic will, since its members face periodic electoral accountability that unelected courts do not.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Whichever coalition commands a legislative majority at a given moment can translate its policy preferences into constitutional meaning without being permanently bound by prior judicial rulings or by a supermajority-entrenched text. Their advantage is temporary and contingent on winning elections, but while in power they face minimal counter-majoritarian check.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majority_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Retain a review function but its output is advisory or easily overridden by ordinary legislative response. Cannot issue a final, binding invalidation that survives a determined legislative majority. Their institutional purpose — settling constitutional questions with finality — is structurally unavailable to them under this reading; they can delay or persuade but not bind.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts, payer,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts, excluded).

% Groups whose rights claims depend on a stable, judicially-enforceable constitutional floor that cannot be legislated away by a passing majority. Under parliamentary primacy, any protection they win in constitutional interpretation remains vulnerable to reversal by ordinary or entrenched statute if a sufficient legislative majority later disagrees. They have no forum with final say that sits above the legislature that may be adverse to them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, entrenched_minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Inherit the same unbound interpretive authority their predecessors held; no legislature can permanently entrench a constitutional meaning against a future legislature's contrary judgment. They benefit from the doctrine in the abstract but are also not present to consent to how a current majority exercises it against future interests.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, future_legislatures, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, future_legislatures, excluded).

% Study how parliamentary-sovereignty systems (Westminster-derivative) compare with judicial-supremacy and coordinate-construction systems in protecting rights, enabling democratic responsiveness, and resisting capture by transient majorities. They document tradeoffs without holding a stake in any one jurisdiction's outcome.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves disputes about ultimate constitutional authority by locating final interpretive say in the body most directly and repeatedly accountable to the electorate, avoiding a standing conflict between an unelected judiciary and an elected legislature over who has the last word.
% TRANSFER_FUNCTION: Moves final interpretive authority over constitutional meaning from courts (and from any judicially-enforced constitutional floor) to whichever coalition currently controls the legislature, and correspondingly moves the durability of rights protections from constitutional entrenchment to ordinary political contest.
% ABSENT_VOICES: Future legislative minorities and rights claimants who might need a constitutional floor immune to majority override are not represented in the current legislative majority's decision to retain unbound interpretive authority; they exist only as future contingencies, not present negotiating parties.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy were abandoned overnight in favor of judicial supremacy or coordinate construction, courts would gain the power to durably bind the legislature, ordinary and entrenched statutes touching constitutional questions would become subject to invalidation without legislative remedy, and political contestation over constitutional meaning would shift from the legislative chamber to the courtroom — a substantial reallocation of where and how constitutional disputes are actually settled.
% FOUNDING_PROBLEM: Historically arose to resolve the question of ultimate sovereignty in systems without a single written supreme-law text (or with a text explicitly subordinated to statute), and to prevent an unelected judiciary from exercising a permanent veto over democratically enacted law — particularly salient in traditions emerging from struggles against monarchical or judicial bodies perceived as insufficiently accountable.
% FOUNDING_PROBLEM_CORROBORATION: Legislators and majoritarian democratic theorists attest the founding problem remains live: unelected judicial finality over contested value questions is still viewed as a democratic deficit. Constitutional courts, comparative scholars, and minority-rights advocates from outside the legislative-majority beneficiary set attest the arrangement now also functions to insulate transient majorities from durable rights constraints, citing instances where legislatures have used their unbound authority to reverse protections previously read into contested constitutional provisions.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.2, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (ε ≈ 0.20 at interval end) is authored low because the primary cost — reduced durability of rights protections against majority reversal — is a real but bounded transfer, not a coercive extraction of resources or labor; the mechanism operates through ordinary democratic contest rather than suppression of exit. Suppression (0.3) reflects that dissenting minorities and courts retain voice (litigation, public advocacy, electoral mobilization) even though they cannot bind the legislature. Theater ratio is low and rises only slightly (0.12→0.20) reflecting modest performative judicial review activity that produces persuasive but non-binding opinions over time — a mild drift toward review theater as courts issue interpretations they know can be legislatively reversed. Accessibility collapse (0.35) is moderate: alternative institutional arrangements (entrenchment, judicial supremacy) remain conceptually and politically available, they are simply not currently adopted. Resistance (0.4) reflects ongoing academic, judicial, and minority-advocacy pressure against the reading without an active suppression apparatus.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, this reading looks like pure democratic coordination — accountability concentrated where the electorate can act on it. From the entrenched-rights-claimant seat, the identical structure looks like a standing vulnerability: whatever protection exists today has no floor beneath a future contrary majority. The engine should compute these as structurally different experiences of the same authority arrangement, driven by the trapped vs. mobile exit-option asymmetry, not by different metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Elected legislature and electoral majority coalitions sit near the beneficiary end: they collect the exercised authority and face only the ordinary electoral check, not judicial override. Entrenched minority rights claimants sit near the target end: they are trapped (no exit from the jurisdiction resolves the vulnerability) and structurally excluded from a durable judicial floor. Constitutional courts sit as payers because the reading directly narrows their institutional function, though their institutional power (institutional) and constrained (not trapped) exit options moderate the derived directionality relative to the rights claimants.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing an unaccountable judiciary from vetoing democratic legislation) remains genuinely live in some polities and genuinely diminished in others where courts have historically exercised restraint anyway — hence founding_problem_status is authored as contested rather than flatly live or dead. This prevents mislabeling the arrangement as either purely functional coordination (ignoring the real cost to rights durability) or purely extractive capture (ignoring that legislative accountability is a real and continuing democratic value, not merely a cover story).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_accountability_vs_majority_capture,
    'Does parliamentary primacy over constitutional meaning function primarily as a genuine safeguard against unaccountable judicial power, or does it function primarily as a mechanism for transient majorities to strip durable protections from minorities who cannot win elections?',
    'Comparative empirical study of Westminster-derivative and similar parliamentary-sovereignty jurisdictions versus judicial-supremacy jurisdictions, tracking rates of legislative reversal of rights-protective interpretations against rates of judicial overreach in the comparator systems, over multiple electoral cycles.',
    'If reversal of rights-protective interpretations by majorities is frequent and disproportionately targets structurally powerless groups, this reading functions closer to a tangled_rope (real democratic coordination function plus asymmetric extraction from a persistent minority) than to a pure rope; if reversal is rare and mostly corrects genuine judicial overreach, the rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_accountability_vs_majority_capture, empirical, 'Whether the reading''s low ε reflects genuine coordination or under-measured majoritarian extraction.').

omega_variable(
    which_reading_is_the_true_kernel_state,
    'Is parliamentary primacy the historically prior, ''natural'' state of the constitutional-authority-boundary kernel from which judicial-supremacy and coordinate-construction readings are later departures, or is it itself one contingent, politically-constructed reading with no privileged claim to being the kernel''s default state?',
    'Constitutional-historical analysis of the specific jurisdiction''s founding documents and subsequent practice: was judicial review explicitly foreclosed at founding, or did parliamentary primacy emerge/persist through subsequent political contest and could have gone otherwise?',
    'If parliamentary primacy is the historically default, uncontested founding arrangement, its low ε and rope classification are well-grounded; if it is itself a later political achievement that displaced a more balanced founding arrangement, the reading''s own legitimacy claim (democratic accountability) may itself be a constructed justification and warrants re-examination alongside the coordinate_construction_reading as the more originally intended arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_true_kernel_state, conceptual, 'Whether this reading''s self-presentation as the kernel''s natural/default state is itself contestable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t8, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(cons_tr_t16, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(cons_tr_t32, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(cons_be_t8, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(cons_be_t16, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement(cons_be_t32, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 32, 0.2).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.2).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(constitutional_authority_boundary__parliamentary_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'constitutional authority boundary' concept into structurally distinct constraints under the ε-invariance principle. parliamentary_primacy_reading (this file) authors low ε (~0.20) reflecting a genuine, bounded democratic-accountability coordination function. judicial_supremacy_reading is expected to author a different ε centered on the risk of unreviewable, unaccountable judicial power over legislative and executive acts. coordinate_construction_reading is expected to author a different ε centered on inter-branch deadlock and the absence of any final arbiter to resolve disputes. All three share the same kernel (the underlying question of who has final say over constitutional meaning) but instantiate different structural claims with different beneficiaries, victims, and extraction profiles — they are linked via network edges, not merged into one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
