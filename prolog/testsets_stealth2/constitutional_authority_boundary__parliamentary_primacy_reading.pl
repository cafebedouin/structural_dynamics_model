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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy over Constitutional Meaning (Westminster Reading)
 *   domain: constitutional/political/institutional
 *
 * SUMMARY:
 *   In constitutional orders descended from the Westminster settlement, the
 *   constitutional text, where it exists in fragmentary statute form, holds
 *   no superior rank over ordinary legislation: the legislature may make,
 *   amend, or unmake any constitutional rule through ordinary or nominally
 *   entrenched legislation, and courts may flag concerns but cannot strike
 *   down primary legislation. This story instantiates the
 *   parliamentary_primacy_reading of the constitutional_authority_boundary
 *   kernel and nothing else. Per the epsilon-referent rule, extractiveness is
 *   authored for the standing arrangement under contest, legislative
 *   finality, as this reading itself sees it: democratically authorized,
 *   self-correcting through elections, hence moderately low, with costs
 *   concentrated on those outside the winning coalition. The sibling readings
 *   are other constraint stories, not positions described inside this one.
 *   The claim (tangled_rope) and the metrics are authored independently: the
 *   claim states what I believe is structurally true, the metrics what I
 *   believe is descriptively true, and the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - governing_parliamentary_majority: Agenda-setter and principal collector (institutional/arbitrage) — holds final interpretive authority for the duration of its mandate
 *   - - elected_legislators_collectively: Beneficiary (institutional/mobile) — the body in which constitutional finality resides
 *   - - electoral_majority_voters: Beneficiary (organized/constrained) — their preferences prevail over any judicial veto
 *   - - electoral_minority_groups: Payer (moderate/trapped) — bear majoritarian policy costs with no judicial remedy; coalition-capable in principle
 *   - - unenfranchised_residents: Payer (powerless/trapped) — subject to legislative finality without vote or veto
 *   - - judiciary_as_institution: Payer (institutional/identity_locked) — constrained to advisory or easily-overridden review
 *   - - constitutional_opposition_parties: Payer/beneficiary (organized/constrained) — position flips with electoral fortune
 *   - - rights_advocacy_organizations: Excluded (organized/constrained) — press for entrenchment from outside the decision rule
 *   - - constitutional_scholars: Observer (analytical/analytical) — map the structure, collect and pay nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.25).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.5).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy over Constitutional Meaning (Westminster Reading)").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional/political/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '4c826fdf-09db-4ff6-a321-2a92be51ec0f').
narrative_ontology:cs_kernel_codification('4c826fdf-09db-4ff6-a321-2a92be51ec0f', implicit).
narrative_ontology:cs_authority_grounding('4c826fdf-09db-4ff6-a321-2a92be51ec0f', practice).
narrative_ontology:cs_interpretation_layer_present('4c826fdf-09db-4ff6-a321-2a92be51ec0f').
narrative_ontology:cs_reading_relation('4c826fdf-09db-4ff6-a321-2a92be51ec0f', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4c826fdf-09db-4ff6-a321-2a92be51ec0f', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_axiom('4c826fdf-09db-4ff6-a321-2a92be51ec0f', foundational, elected_representatives_hold_final_constitutional_authority).
narrative_ontology:cs_axiom_status(elected_representatives_hold_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('4c826fdf-09db-4ff6-a321-2a92be51ec0f', elected_representatives_hold_final_constitutional_authority, conventional).
narrative_ontology:cs_axiom('4c826fdf-09db-4ff6-a321-2a92be51ec0f', secondary, judicial_determinations_yield_to_legislative_reconsideration).
narrative_ontology:cs_axiom_status(judicial_determinations_yield_to_legislative_reconsideration, holdable).
narrative_ontology:cs_axiom_grounding('4c826fdf-09db-4ff6-a321-2a92be51ec0f', judicial_determinations_yield_to_legislative_reconsideration, instrumental).
narrative_ontology:cs_reference_frame('4c826fdf-09db-4ff6-a321-2a92be51ec0f', legislative_supremacy_framework).
narrative_ontology:cs_drift_state('4c826fdf-09db-4ff6-a321-2a92be51ec0f', contemporary_post_brexit_reassertion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c826fdf-09db-4ff6-a321-2a92be51ec0f', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislators_collectively).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majority_voters).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_minority_groups).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, unenfranchised_residents).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary_as_institution).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_opposition_parties).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_opposition_parties).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_accountability_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, popular_will_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds working control of the legislature for the duration of its mandate. Sets the legislative agenda, drafts and passes constitutional-significance statutes through ordinary procedure, and answers to no court on primary legislation. When its reading of a constitutional rule conflicts with a court's, it can legislate its reading into effect. Its position lasts exactly as long as its majority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majority, beneficiary).

% The body in which final lawmaking authority resides. Any member can move amendment or repeal of any constitutional rule; collectively they can remake the constitutional settlement in an afternoon of ordinary business. Individual members rise and fall with elections, but the body's authority does not lapse between parliaments.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislators_collectively, beneficiary,
    institutional, biographical, mobile, national).

% Citizens whose preferred coalition currently wins elections. Their choices translate directly into constitutional outcomes because nothing stands between their legislature and the statute book. Their protection is the next election, not a court; when their coalition loses, they join the ranks of the outvoted.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_majority_voters, beneficiary,
    organized, biographical, constrained, national).

% Cohorts that lose elections persistently or by wide margins; constitutional-significant policy is enacted over their objection whenever a majority forms against them. Their recourse is persuasion of future majorities, supermajority argument, and protest. No court can halt a statute once passed. Some cohorts are well-resourced and capable of coalition; others are small, dispersed, or newly arrived.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, electoral_minority_groups, payer,
    moderate, biographical, trapped, national).

% People subject to the legislature's final authority who hold no vote: resident non-citizens, children, disenfranchised adults. Laws of constitutional significance apply to them in full; they have neither the ballot nor any judicial brake on enactment. Their interests enter the process only through advocacy by others.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, unenfranchised_residents, payer,
    powerless, biographical, trapped, national).

% The court system. It interprets statutes, flags incompatibilities with rights instruments, and can sometimes police delegated legislation, but on primary legislation its determinations yield: a declaration of concern can be answered by legislative restatement, and the doctrine of legislative supremacy bars invalidation. Generations of judges are trained into the deference convention; departing from it is professionally costly and constitutionally explosive, so the bench polices its own restraint.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary_as_institution, payer,
    institutional, generational, identity_locked, national).

% Parties currently out of power. They bear the majority's constitutional interpretations today and expect to wield the same finality after the next victory, so they campaign against particular uses of legislative finality while defending the finality itself. Their incentives alternate with electoral fortune.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_opposition_parties, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_opposition_parties, beneficiary).

% Campaign groups pressing for entrenched bills of rights, stronger judicial protection, or a codified constitution. They testify, litigate at the margins, and shape public opinion, but they hold no seat in the decision rule: their proposals take effect only if a future majority chooses to enact them.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, rights_advocacy_organizations, excluded,
    organized, generational, constrained, national).

% Academic and comparative analysts of constitutional design. They map how the arrangement behaves, document drift between doctrine and practice, and advise reformers on all sides; they collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, governing_parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the who-decides problem when institutions disagree about constitutional meaning: final interpretive authority is routed to the elected legislature, so constitutional disputes resolve through the same electoral-legislative process as ordinary politics rather than through inter-branch stalemate or judicial fiat.
% TRANSFER_FUNCTION: Moves interpretive authority over constitutional questions from courts and constitutional texts to the sitting legislative majority; correspondingly moves rights-security away from electoral minorities, who lose any judicial backstop, toward majority-preference satisfaction.
% ABSENT_VOICES: Unenfranchised residents, children, and future generations would object that final authority holds their interests hostage to shifting majorities; they are absent because they hold no votes and no seats. Persistent electoral minorities are present in numbers but structurally outvoted. Rights-advocacy organizations press from outside the decision rule and are heard only as lobbying, never as a veto.
% DISAPPEARANCE_RATIONALE: If legislative finality vanished overnight, the vacuum would be filled by one of the rival allocations: courts striking down acts, or negotiated inter-branch settlement. Every statute of constitutional significance would become newly contestable, devolution and rights settlements would wobble, and the legislative agenda would reorganize around re-founding the boundary itself.
% FOUNDING_PROBLEM: After the seventeenth-century conflicts between crown, courts, and parliament, the settlement had to fix definitively that the elected legislature, not the monarch and not judges invoking supra-legislative principles, holds final lawmaking authority, so that democratic legislation could not be vetoed by unelected bodies.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Glorious Revolution settlement and comparative constitutional scholars corroborate that the founding problem was real and urgent. Corroboration of its CURRENT status comes from outside the benefiting parties: minority-rights organizations and international human-rights bodies attest the problem survives in transformed form (who protects those who reliably lose the vote), while the benefiting parties' own memoranda cite judicial-overreach risk in its original form and are discounted accordingly as self-interested attestation.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is 0.25: genuine democratic authorization keeps the arrangement far below coerced-extraction levels, but finality without judicial backstop imposes real, recurring costs on persistent losers, so it is not negligible either. Suppression is 0.50 and structural-legal rather than violent: courts are bound by the doctrine of legislative supremacy and implied repeal, and defiance would simply be legislated past. Theater is 0.24: the doctrine does real work daily, but a growing share of sovereignty assertion is restatement-for-effect, notably sovereignty-clause statutes that restate an already-true fact. Accessibility collapse is 0.60: within the framework, strong-form judicial review is foreclosed, yet codified and entrenched alternatives are periodically proposed and debated rather than unthinkable. Resistance is 0.40: sustained minority-rights campaigning and the common-law constitutionalist school in the academy and occasionally the bench, insufficient to dislodge the doctrine. All three temporal series run on one shared six-point grid (1688, 1832, 1911, 1949, 1998, 2026). The 1949 extractiveness peak reflects post-war single-party dominance enacting sweeping constitutional-significant legislation; the 1998 dip reflects the rights-dialogue layer softening raw subordination; the terminal rise reflects post-2016 reassertion of finality. Receipt surface: the gains demonstrably accrue to the sitting majority, which is named; fixing, meaning replacing legislative finality with a codified or judicially arbitrated boundary, is procedurally trivial for the very seat that collects the gains but politically prohibitive, since it requires the collector to surrender its own advantage.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat should compute the arrangement as empowerment approaching subsidy: it faces no binding constraint and writes the rules. The minority payer seats compute enforced exposure: the same finality that empowers the majority is the mechanism that enacts policy over their objection with no recourse. The judiciary's seat is the sharpest divergence: high nominal institutional power combined with identity-locked exit yields high experienced burden despite high power, because the fusion of professional identity with the deference convention means the bench cannot convert its authority into exit. Opposition parties straddle the gap: today's payer is tomorrow's collector, which damps their incentive to resist the arrangement itself and helps explain its persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (the governing majority, the legislative body collectively, current majority voters) derive low directionality, toward the subsidized end. Victims derive high directionality; among them, trapped and identity-locked exits (minorities, the unenfranchised, the judiciary) sit nearer the full-target end than the merely constrained opposition parties, whose dual position averages their exposure. Spatial scope is national, where verification of what the legislature has enacted is comparatively easy, moderating the scope amplification. No directionality overrides were needed: the beneficiary and victim declarations together with the exit atoms produce an accurate structural relationship for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, settling crown-and-court against parliament after the seventeenth-century conflicts, remains live in transformed form: the who-decides-when-institutions-disagree problem recurs with every new rights instrument and every assertive court. No mandatrophy declaration is made. The classification guards against both mislabeling errors: calling this a pure extraction arrangement would erase the genuine coordination function, routing constitutional disagreement through the democratic process instead of inter-branch stalemate; calling it pure coordination would erase the asymmetric cost-bearing, in which persistent losers bear rights costs without remedy through the same structure that coordinates everyone else. The hybrid claim captures both halves, and the receipt surface, gains accruing to the sitting majority, marks the asymmetric half without collapsing the coordination half.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_allocation_underdetermination,
    'Which allocation of final constitutional interpretive authority does the standing order actually embody: legislative finality (this reading), judicial finality, or distributed co-equality?',
    'Observe decisive cases: whether courts ever invalidate primary legislation without a legislative path to reversal, and whether any branch''s interpretation routinely prevails across spheres.',
    'If judicial finality obtains, this story''s victim set expands to include the legislature and executive and measured extraction rises sharply; if coordinate construction obtains, no single seat captures the gains and the receipt surface becomes diffuse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_allocation_underdetermination, conceptual, 'Committer-frame omega: this constraint is one reading of kernel constitutional_authority_boundary (parliamentary_primacy_reading); sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    minority_cost_separability,
    'Is the rights-cost borne by electoral minorities an inherent price of legislative finality, or separable through entrenched-rights mechanisms compatible with parliamentary primacy such as dialogue models and manner-and-form entrenchment?',
    'Comparative analysis of jurisdictions retaining legislative finality while adding rights-dialogue layers: measure whether minority adverse outcomes fall without transferring finality to courts.',
    'If separable, excess extraction falls toward pure-coordination territory; if inherent, the hybrid classification hardens and suppression requirements rise as minorities press harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_cost_separability, empirical, 'Whether the coordination and cost-bearing components of legislative finality are structurally separable.').

omega_variable(
    judicial_deference_trajectory,
    'Will judicial deference to legislative finality harden under backlash dynamics, or erode as common-law constitutionalism asserts substantive limits?',
    'Track the suppression_requirement series beyond 2026 alongside doctrinal markers: frequency of rights-flags, outcomes of executive-defiance litigation, appointment philosophies.',
    'Hardening pushes the arrangement toward harsher operation for minority seats; erosion pulls practice toward distributed authority regardless of formal doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_trajectory, empirical, 'Direction of enforcement-capacity drift for the subordination of courts.').

omega_variable(
    entrenchment_possibility_ambiguity,
    'Can a sovereign legislature genuinely entrench constitutional rules against future parliaments, or does substantive entrenchment remain impossible, leaving ''entrenched legislation'' nominal?',
    'Doctrinal analysis and stress events: whether any manner-and-form or referendum-lock provision survives a determined majority''s attempt to repeal it.',
    'If entrenchment is impossible, future generations and minorities face unlimited reversability, raising effective extraction; if possible, protected transitional elements exist inside the arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(entrenchment_possibility_ambiguity, conceptual, 'Whether the ordinary-versus-entrenched legislation distinction is structurally real.').

omega_variable(
    majority_use_profile,
    'Does the sitting majority deploy final interpretive authority predominantly for coordination (stable governance, credible commitment) or for displacement of persistent losers?',
    'Legislative-history coding of constitutional-significance statutes across alternating governments: the proportion whose primary incidence falls on identifiable minority cohorts.',
    'A displacement-dominant profile supports upgrading the payer seats'' computed severity; a coordination-dominant profile supports this reading''s low-extraction self-assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_use_profile, empirical, 'Empirical profile of how legislative finality is actually used across successive governments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 1688, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cap_ppr_tr_t1688, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1688, 0.1).
narrative_ontology:measurement_basis(cap_ppr_tr_t1688, observed).
narrative_ontology:measurement(cap_ppr_tr_t1832, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1832, 0.12).
narrative_ontology:measurement_basis(cap_ppr_tr_t1832, observed).
narrative_ontology:measurement(cap_ppr_tr_t1911, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1911, 0.18).
narrative_ontology:measurement_basis(cap_ppr_tr_t1911, observed).
narrative_ontology:measurement(cap_ppr_tr_t1949, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1949, 0.22).
narrative_ontology:measurement_basis(cap_ppr_tr_t1949, observed).
narrative_ontology:measurement(cap_ppr_tr_t1998, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement_basis(cap_ppr_tr_t1998, observed).
narrative_ontology:measurement(cap_ppr_tr_t2026, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 2026, 0.24).
narrative_ontology:measurement_basis(cap_ppr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(cap_ppr_be_t1688, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1688, 0.14).
narrative_ontology:measurement_basis(cap_ppr_be_t1688, observed).
narrative_ontology:measurement(cap_ppr_be_t1832, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1832, 0.17).
narrative_ontology:measurement_basis(cap_ppr_be_t1832, observed).
narrative_ontology:measurement(cap_ppr_be_t1911, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1911, 0.21).
narrative_ontology:measurement_basis(cap_ppr_be_t1911, observed).
narrative_ontology:measurement(cap_ppr_be_t1949, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1949, 0.27).
narrative_ontology:measurement_basis(cap_ppr_be_t1949, observed).
narrative_ontology:measurement(cap_ppr_be_t1998, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 1998, 0.23).
narrative_ontology:measurement_basis(cap_ppr_be_t1998, observed).
narrative_ontology:measurement(cap_ppr_be_t2026, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 2026, 0.25).
narrative_ontology:measurement_basis(cap_ppr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(cap_ppr_su_t1688, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1688, 0.3).
narrative_ontology:measurement_basis(cap_ppr_su_t1688, observed).
narrative_ontology:measurement(cap_ppr_su_t1832, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1832, 0.35).
narrative_ontology:measurement_basis(cap_ppr_su_t1832, observed).
narrative_ontology:measurement(cap_ppr_su_t1911, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1911, 0.4).
narrative_ontology:measurement_basis(cap_ppr_su_t1911, observed).
narrative_ontology:measurement(cap_ppr_su_t1949, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1949, 0.48).
narrative_ontology:measurement_basis(cap_ppr_su_t1949, observed).
narrative_ontology:measurement(cap_ppr_su_t1998, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement_basis(cap_ppr_su_t1998, observed).
narrative_ontology:measurement(cap_ppr_su_t2026, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 2026, 0.5).
narrative_ontology:measurement_basis(cap_ppr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional authority' decomposes into three structurally distinct claims about where final interpretive authority resides. This story is the parliamentary-primacy member of the family: its epsilon is authored for legislative finality as this reading sees it. The judicial-supremacy sibling and the coordinate-construction sibling are separate files with their own epsilon, victim sets, and classifications; the edges here record family membership, not internal contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
