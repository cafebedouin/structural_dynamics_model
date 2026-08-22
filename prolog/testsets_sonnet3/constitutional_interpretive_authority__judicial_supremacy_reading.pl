% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel of
 *   constitutional interpretive authority: the judicial supremacy reading,
 *   under which courts possess final say over constitutional meaning and may
 *   nullify legislative acts that conflict with their interpretation. Under
 *   this reading, the judiciary is a structural beneficiary of interpretive
 *   authority, the litigation bar that services constitutional claims is a
 *   co-beneficiary, and legislative majorities together with the electoral
 *   constituencies they represent are structurally subordinated whenever a
 *   judicial reading overrides an enacted statute. The coordination function
 *   this reading offers is real (protecting entrenched rights from transient
 *   majoritarian repeal), but the same structure imposes a cost on democratic
 *   responsiveness that falls unevenly, especially on populations who cannot
 *   access litigation. This is a Tangled Rope: courts genuinely solve a
 *   coordination problem (credible commitment to constitutional limits) while
 *   the same mechanism extracts final say from elected bodies and requires
 *   active enforcement (judicial review, contempt power, executive
 *   compliance) to hold against legislative resistance.
 *
 * KEY AGENTS:
 *   - constitutional_judiciary: primary beneficiary and agenda-setter (institutional/arbitrage) — sets interpretive doctrine and exercises nullification power
 *   - legislative_majorities: primary payer (organized/constrained) — enacted statutes subject to override
 *   - electoral_constituencies_overridden: diffuse payer (powerless/trapped) — democratic preference subordinated with no direct recourse
 *   - rights_claimants_with_standing: secondary beneficiary (moderate/mobile) — gains a forum insulated from majoritarian politics
 *   - minority_populations_without_litigation_access: nominal beneficiary who is structurally a payer (powerless/trapped) — invoked as rationale, rarely reached in practice
 *   - constitutional_litigation_bar: secondary beneficiary (organized/arbitrage) — professional stake in judicial supremacy persisting
 *   - legislature_as_institution: excluded voice (institutional/constrained) — no binding forum to contest specific interpretations
 *   - comparative_constitutional_scholars: analytical observer (analytical/global) — compares regimes across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.52).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'f3edabe5-59d6-435d-a300-257e0c8c2235').
narrative_ontology:cs_kernel_codification('f3edabe5-59d6-435d-a300-257e0c8c2235', formalized).
narrative_ontology:cs_authority_grounding('f3edabe5-59d6-435d-a300-257e0c8c2235', lineage).
narrative_ontology:cs_interpretation_layer_present('f3edabe5-59d6-435d-a300-257e0c8c2235').
narrative_ontology:cs_reading_relation('f3edabe5-59d6-435d-a300-257e0c8c2235', constitutional_interpretive_authority__parliamentary_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f3edabe5-59d6-435d-a300-257e0c8c2235', constitutional_interpretive_authority__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('f3edabe5-59d6-435d-a300-257e0c8c2235', foundational, judicial_finality_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f3edabe5-59d6-435d-a300-257e0c8c2235', judicial_finality_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('f3edabe5-59d6-435d-a300-257e0c8c2235', foundational, counter_majoritarian_rights_protection_justifies_nullification).
narrative_ontology:cs_axiom_status(counter_majoritarian_rights_protection_justifies_nullification, holdable).
narrative_ontology:cs_axiom_grounding('f3edabe5-59d6-435d-a300-257e0c8c2235', counter_majoritarian_rights_protection_justifies_nullification, instrumental).
narrative_ontology:cs_reference_frame('f3edabe5-59d6-435d-a300-257e0c8c2235', marbury_judicial_review_settlement).
narrative_ontology:cs_drift_state('f3edabe5-59d6-435d-a300-257e0c8c2235', contemporary_polarized_appointments_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f3edabe5-59d6-435d-a300-257e0c8c2235', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_litigation_bar).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claimants_with_standing).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_constituencies_overridden).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, minority_populations_without_litigation_access).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_rights_protection_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on whether legislation is constitutionally valid, exercised through judicial review and the power to nullify statutes. Sets the interpretive doctrine (textualism, living constitutionalism, proportionality tests) that determines outcomes, and is not itself subject to electoral removal for its interpretive choices. Accrues institutional authority and deference each time a nullification is accepted rather than resisted.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary, beneficiary).

% Enacts statutes reflecting electoral mandates, which can be voided or narrowed by judicial ruling on constitutional grounds. Can respond through constitutional amendment (typically supermajority-gated and slow) or court-packing/jurisdiction-stripping (politically costly and norm-breaking). Bears the cost of having its democratic output subordinated to an unelected body's reading of foundational text.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Voted for representatives who passed a law later struck down by courts applying a constitutional interpretation the constituency had no direct part in selecting. Their electoral preference is subordinated to a judicial reading they cannot appeal to the ballot box; recourse requires generational-scale constitutional amendment or new judicial appointments over decades.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, electoral_constituencies_overridden, payer,
    powerless, biographical, trapped, national).

% Individuals or groups whose rights claims survive majoritarian politics only because courts can nullify legislation that would otherwise extinguish them. Gain a forum insulated from ordinary vote-counting, provided they can access litigation resources and satisfy standing doctrine.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, rights_claimants_with_standing, beneficiary,
    moderate, biographical, mobile, national).

% Nominally protected by the same rights-guardianship rationale, but lack the resources, standing, or legal sophistication to bring claims that would trigger judicial protection. The theoretical benefit of judicial supremacy does not reach them in practice, while they remain bound by whatever the interpretive regime produces for others.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, minority_populations_without_litigation_access, payer,
    powerless, biographical, trapped, national).

% Specialist lawyers, advocacy organizations, and academics whose professional and financial position depends on the constitution being a live, judicially enforceable document rather than a political text settled by legislatures. Benefits from every high-stakes nullification case regardless of outcome.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_litigation_bar, beneficiary,
    organized, generational, arbitrage, national).

% As an institution (distinct from any sitting majority), has no formal channel to contest a specific judicial interpretation short of amendment or a fresh statute inviting further litigation. Would argue that its democratic mandate should carry interpretive weight equal to or greater than judicial doctrine, but has no forum in which to make that argument binding.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legislature_as_institution, excluded,
    institutional, generational, constrained, national).

% Study how judicial supremacy operates across jurisdictions relative to parliamentary supremacy and coordinate construction regimes, tracing which populations gain rights protection and which lose democratic responsiveness under each arrangement.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_judiciary).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, depoliticized forum for adjudicating whether legislative action violates entrenched constitutional commitments, preventing transient majorities from unilaterally eliminating rights or restructuring foundational arrangements without extraordinary process.
% TRANSFER_FUNCTION: Moves final interpretive authority over contested constitutional meaning from elected legislatures to appointed or life-tenured judges; moves practical rights protection toward litigants with standing and resources, away from electoral majorities and away from populations who cannot access courts.
% ABSENT_VOICES: Electoral constituencies whose enacted preferences are nullified have no forum to contest the interpretation itself, only slow recourse through amendment or future appointments. Minority populations lacking litigation resources are invoked as the rationale for the arrangement but often cannot access its protections.
% DISAPPEARANCE_RATIONALE: If judicial nullification power vanished overnight, legislative majorities would face no constitutional check beyond political self-restraint; statutes currently voided or chilled by anticipated judicial review would proceed; the litigation bar's core practice area would collapse; and rights currently secured against majoritarian repeal (e.g., protections upheld only through nullification of contrary legislation) would depend entirely on ordinary political majorities to persist.
% FOUNDING_PROBLEM: Written constitutions claiming supremacy over ordinary legislation need an enforcement mechanism, or the supremacy claim is merely aspirational; courts were positioned to fill that role because legislatures cannot credibly be trusted to police their own constitutional limits.
% FOUNDING_PROBLEM_CORROBORATION: Judges and constitutional scholars who work within the judicial-supremacy tradition attest the problem remains live (legislatures do periodically pass rights-infringing statutes). Political theorists working from parliamentary-supremacy and coordinate-construction traditions — outside the judiciary's own institutional interest — argue the mechanism has substituted judicial policy preference for the rights-enforcement function it claims, particularly where doctrine has drifted from constitutional text; comparative scholarship on court-packing and appointment politics in multiple jurisdictions supports the contested reading.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) reflects that judicial nullification transfers a real and consequential authority away from elected bodies, but the transfer is bounded by doctrine, precedent, and the possibility of amendment or appointment change — it is not unbounded extraction. Suppression (0.58) is moderate-high: legislatures retain the formal power to pass statutes and pursue amendment, but any statute conflicting with judicial doctrine is functionally unenforceable until doctrine shifts, and resistance mechanisms (court-packing, jurisdiction-stripping) carry high political cost, which is itself a form of suppression of the alternative. Theater ratio (0.28) is present but not dominant: doctrinal reasoning does real interpretive work, though a portion of judicial opinion-writing serves legitimation rather than decision (post-hoc rationalization of outcomes reached on other grounds). accessibility_collapse (0.62) reflects that once judicial supremacy is established as the operating norm, alternative arrangements (parliamentary supremacy, coordinate construction) become difficult to reintroduce without a constitutional crisis or amendment. Resistance (0.55) is substantial because legislative majorities and constituencies do actively contest specific rulings, propose court reform, and litigate around doctrine, even though few of these efforts succeed in altering the underlying interpretive-authority allocation. All three temporal metrics run on one shared grid (0/20/40/60/80/100) reflecting the gradual entrenchment of judicial review as doctrine accumulates and precedent forecloses legislative reversal.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this reading appears as principled rights-guardianship performing exactly the function constitutions require. From the legislative-majority seat, the identical structure appears as an unelected body substituting its judgment for the electorate's, defended by doctrine the legislature had no hand in shaping. The engine computes these divergent seat-level classifications from the same structural data; the story does not adjudicate which seat is correct, only what the structure is.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits nearest the full-beneficiary end: it collects interpretive authority, is largely insulated from removal for its rulings, and each accepted nullification strengthens the norm that judicial review is legitimate. The litigation bar and standing-holding rights claimants are secondary beneficiaries with genuine but narrower gains. Legislative majorities and the constituencies they represent sit toward the target end: their enacted preferences can be voided, and their remedies (amendment, appointment politics) are slow and costly. Minority populations without litigation access are formally invoked as beneficiaries of the rights-guardianship rationale but structurally experience the arrangement as payers, since the protective mechanism does not reach them without resources they lack — this gap is itself part of what the ε assessment captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The judicial-supremacy reading resists mandatrophy mislabeling in both directions: treating it as pure coordination (a Rope) would erase the real cost imposed on democratic majorities and the uneven access to its protections; treating it as pure extraction (a Snare) would erase the genuine function of protecting entrenched rights from majoritarian repeal, which is not merely a cover story — some populations demonstrably retain protections that ordinary legislative majorities have tried to remove. The Tangled Rope classification holds both: real coordination (credible constitutional commitment) and real asymmetric extraction (final say moved from elected bodies to appointed judges, unevenly available in practice) coexisting in the same structure, requiring active enforcement to persist against legislative resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is judicial supremacy the correct reading of constitutional interpretive authority, or is it one contested reading among parliamentary supremacy and coordinate construction, with no framework-external fact settling which is correct?',
    'This is not empirically resolvable by data internal to any one reading — it is a jurisprudential/political-theory dispute about where interpretive authority ought to sit. The corpus models it by generating each reading as a separate constraint (this story, parliamentary_supremacy_reading, coordinate_construction_reading) linked via network.affects_constraints, rather than adjudicating between them within one story.',
    'If a jurisdiction''s actual constitutional practice shifts (e.g., toward court-packing that neuters judicial review, or toward legislative override clauses), the operative reading changes and a different sibling constraint becomes the descriptively accurate one for that jurisdiction at that time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which reading of the interpretive-authority kernel is structurally correct is itself contested and not resolved within this story.').

omega_variable(
    doctrine_vs_policy_preference,
    'When courts nullify legislation, is the outcome driven by genuine constitutional-text/structure constraint, or by judges'' policy preferences dressed in doctrinal language?',
    'Empirical political-science research on judicial voting patterns correlated with appointing-party ideology, controlling for doctrinal area and case facts; convergence or divergence of outcomes across differently-composed courts applying the same doctrine to similar facts.',
    'If outcomes track appointing-party ideology more than doctrinal text, the theater_ratio and extractiveness for this reading should be revised upward — nullification would function more as policy substitution than as rights-guardianship. If outcomes track text/precedent robustly across court compositions, the coordination function is stronger than the current metrics assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_policy_preference, empirical, 'Whether judicial nullification tracks genuine constitutional constraint or judicial policy preference.').

omega_variable(
    access_gap_severity,
    'How large is the gap between the theoretical rights-protection this reading promises and the practical protection reached by populations without litigation resources?',
    'Comparative data on standing doctrine restrictiveness, legal aid availability, and case outcomes for resourced vs. under-resourced claimants across jurisdictions operating under judicial supremacy.',
    'A wide gap would strengthen the tangled_rope classification (coordination function real but unevenly delivered, extraction from underserved populations who bear costs of the regime without its benefits); a narrow gap would push the reading closer to a genuine Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_gap_severity, empirical, 'Whether the rights-protection rationale reaches the populations it claims to protect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t20, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(cons_tr_t60, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(cons_tr_t80, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(cons_tr_t100, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t20, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(cons_be_t60, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(cons_be_t80, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(cons_be_t100, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cons_su_t20, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.49).
narrative_ontology:measurement(cons_su_t60, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(cons_su_t80, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(cons_su_t100, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the constitutional_interpretive_authority kernel. judicial_supremacy_reading (this story) authors courts as beneficiaries and legislatures as payers with a tangled_rope classification (real rights-protection coordination plus real democratic-authority extraction). parliamentary_supremacy_reading authors the legislature as the terminal authority with no judicial nullification power, inverting the beneficiary/payer structure. coordinate_construction_reading authors neither branch as final, instead modeling inter-branch dialogue — its ε and stakeholder structure differ again, since no single actor holds the disputed authority. Each story's ε is stable and specific to its own reading's structural claims; none averages across the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
