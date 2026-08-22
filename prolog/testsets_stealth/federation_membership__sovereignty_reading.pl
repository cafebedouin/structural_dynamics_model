% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership as Conditional Treaty (Sovereignty Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   A federation whose members pool economic institutions while each national
 *   government retains final authority over who crosses its borders:
 *   membership operates as a conditional treaty, and free movement functions
 *   as a negotiated policy that governments can narrow, suspend, or price
 *   through visas, quotas, and safeguard clauses. Protected-side seats
 *   (incumbent labor markets, control bureaucracies, governments collecting
 *   political returns from control) coexist with burden-bearing seats (mobile
 *   citizens, frontier workers, third-country nationals) whose status is
 *   revocable and whose compliance costs fund the arrangement's operation.
 *   This file instantiates ONE reading of the contested federation_membership
 *   kernel — the sovereignty_reading — and authors epsilon for the standing
 *   conditional-membership arrangement as that reading assesses it. The
 *   sibling integration_reading is a separate constraint with its own victim
 *   set and its own epsilon; the contest between readings is routed to omega
 *   variables, not averaged here. Claim and metrics are independent authored
 *   facts: the constraint is CLAIMED as tangled_rope from this seat, while
 *   the metrics describe its observed operation. KEY AGENTS (by structural
 *   relationship): - national_governments: Agenda setter
 *   (institutional/constrained) — administers admission, collects political
 *   returns from control - domestic_labor_market_incumbents: Primary
 *   beneficiary (organized/constrained) — shielded wages and welfare pools -
 *   immigration_control_bureaucracies: Secondary beneficiary and
 *   administrator (institutional/constrained) - mobile_citizens: Primary
 *   target (moderate/constrained) — bears mobility restriction across
 *   jurisdictions - cross_border_workers: Concentrated target
 *   (powerless/trapped) — income severed by any closure -
 *   third_country_nationals: Most exposed target (powerless/trapped) —
 *   revocable status, enforcement falls hardest here -
 *   export_oriented_employers: Excluded voice (powerful/arbitrage) — would
 *   expand admission, sidelined by framing - supranational_court_judges:
 *   Analytical observer (institutional/analytical) — adjudicates how
 *   negotiable movement really is
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.65).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership as Conditional Treaty (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'd2fc2661-cf70-429d-9435-b5b22399dd10').
narrative_ontology:cs_kernel_codification('d2fc2661-cf70-429d-9435-b5b22399dd10', fixed_text).
narrative_ontology:cs_authority_grounding('d2fc2661-cf70-429d-9435-b5b22399dd10', lineage).
narrative_ontology:cs_interpretation_layer_present('d2fc2661-cf70-429d-9435-b5b22399dd10').
narrative_ontology:cs_reading_relation('d2fc2661-cf70-429d-9435-b5b22399dd10', federation_membership__integration_reading, forecloses).
narrative_ontology:cs_axiom('d2fc2661-cf70-429d-9435-b5b22399dd10', foundational, movement_entitlement_derives_from_interstate_consent).
narrative_ontology:cs_axiom_status(movement_entitlement_derives_from_interstate_consent, holdable).
narrative_ontology:cs_axiom_grounding('d2fc2661-cf70-429d-9435-b5b22399dd10', movement_entitlement_derives_from_interstate_consent, conventional).
narrative_ontology:cs_axiom('d2fc2661-cf70-429d-9435-b5b22399dd10', foundational, final_admission_authority_rests_with_member_states).
narrative_ontology:cs_axiom_status(final_admission_authority_rests_with_member_states, holdable).
narrative_ontology:cs_axiom_grounding('d2fc2661-cf70-429d-9435-b5b22399dd10', final_admission_authority_rests_with_member_states, deontological).
narrative_ontology:cs_reference_frame('d2fc2661-cf70-429d-9435-b5b22399dd10', interstate_consent_compact).
narrative_ontology:cs_drift_state('d2fc2661-cf70-429d-9435-b5b22399dd10', contemporary_post_entrenchment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2fc2661-cf70-429d-9435-b5b22399dd10', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, domestic_labor_market_incumbents).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, immigration_control_bureaucracies).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, cross_border_workers).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, third_country_nationals).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(federation_membership__sovereignty_reading, national_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate the terms under which their state participates in the federation, legislate who may enter and work in the national territory, and operate the visa, border, and removal systems that give those rules effect. They invoke safeguard clauses to suspend movement during crises and answer to domestic electorates for the degree of control exercised. Pooled obligations limit what they can change unilaterally; full revision requires treaty amendment or withdrawal.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, biographical, constrained, national).

% Resident workers, benefit recipients, and their representatives whose wages and social-insurance pools are shielded from unlimited labor competition by national admission rules. They vote in the elections that decide movement policy and are geographically concentrated, which gives their preferences weight in national politics. Some members work abroad themselves, but relocation is costly and a minority practice.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, domestic_labor_market_incumbents, beneficiary,
    organized, biographical, constrained, regional).

% Interior ministries, migration offices, and border agencies that administer permits, run checks, and carry out removals. Restriction expands their staffing, budgets, and statutory mandates; liberalization contracts them. They draft the operational detail that determines how strict the rules are in practice.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, immigration_control_bureaucracies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, immigration_control_bureaucracies, agenda_setter).

% Citizens of member states who live, work, or study in another member state. Their ability to reside and be employed rests on permits, registration rules, and bilateral arrangements that host-state governments can revise. They generally cannot vote in the elections that set those terms and are dispersed across jurisdictions, which limits their capacity to act collectively.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, continental).

% Frontier and seasonal workers whose income depends on regular passage between a home community and a workplace across a national border. Permit backlogs, checkpoint closures, or quota suspensions cut their earnings immediately. Relocating home or workplace to one side of the border is usually unaffordable.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, cross_border_workers, payer,
    powerless, immediate, trapped, regional).

% People who are not citizens of any member state and live or work inside one under national admission schemes. Their status is granted case by case, is easier to revoke than citizenship, and exposes them to detention and removal when rules tighten. Family reunification and permanent settlement depend on discretionary decisions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, third_country_nationals, payer,
    powerless, biographical, trapped, continental).

% Firms in agriculture, care, logistics, and manufacturing that rely on migrant labor and face vacancies when admission narrows. They press governments for larger quotas and faster processing but are marginal voices in negotiations framed around control and security. Their capital is mobile: they can shift production to lower-friction jurisdictions instead.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, export_oriented_employers, excluded,
    powerful, biographical, arbitrage, continental).

% Judges on the federation's court who decide conflicts between treaty mobility provisions and national safeguard measures. Their rulings determine how easily member states may restrict movement in practice. They hold office independently of the governments whose measures they review.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_court_judges, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, domestic_labor_market_incumbents).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates admission to a shared economic space through nationally retained gatekeeping: each member state decides who may enter its territory and on what terms, allowing pooled institutions elsewhere while keeping the composition of the national labor force and welfare pool under domestic democratic control.
% TRANSFER_FUNCTION: Moves decision rights over human mobility from individuals and supranational bodies to national governments; moves compliance costs (permits, checks, waiting times, legal insecurity) onto everyone crossing borders; preserves wage and welfare-pool advantages for incumbent residents.
% ABSENT_VOICES: Mobile citizens and third-country nationals have no seat in the councils where movement terms are set: they are governed by states in which they cannot vote. Export-oriented employers and cross-border regions are consulted intermittently but subordinated whenever the negotiation is framed as control and security. Their absence is what lets unanimity among governments read as consensus.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — membership became unconditional and movement an entrenched entitlement — border and visa regimes would dissolve within months, wages and rents in protected sectors would reprice against open competition, enforcement bureaucracies would contract, and political authority over membership would migrate to supranational institutions. Every named seat's situation depends on the arrangement persisting.
% FOUNDING_PROBLEM: How can distinct sovereign peoples pool markets and institutions without surrendering final authority over who belongs — reconciling economic integration with national self-determination over membership and admission.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: supranational court jurisprudence, academic federalism scholarship, and the integration_reading's own proponents all attest that the founding tension between pooled markets and national control of belonging remains unresolved — they dispute its proper resolution, not its existence. No party to the dispute claims the founding problem has been dissolved.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68) because restriction prices mobility through permits, quotas, and waiting times that are decoupled from any demonstrated scarcity of capacity — the cost falls on movers while the benefit concentrates on protected-side seats. Suppression (0.65) reflects that persistence depends on active machinery — border infrastructure, visa regimes, removal operations, and the exclusion of rival admission channels — not on participant preference; the mechanism is predominantly structural (legal barriers, roughly four-fifths of the total) with a smaller internalized component (public acceptance of border legitimacy that dampens resistance). Theater ratio (0.32) is moderate: screening and removals function, but a growing share of activity is symbolic sovereignty performance — headline quotas, control declarations, crisis summits — that alters little on the ground. Accessibility collapse is moderate (0.45): alternatives remain live and practiced — open-admission bilateral accords, expansive judicial readings, the sibling integration_reading itself — so the construct does not present as natural law. Resistance (0.55) is continuous: litigation by mobile citizens, employer pressure, integrationist parties, and adverse court rulings. The three temporal series share one grid (decadal points 0-60); the monotone rise models a ratchet in which each crisis episode leaves enforcement infrastructure in place after the crisis passes — electoral-cycle oscillation exists but is smoothed at decadal sampling, and the ratchet, not the cycle, is the load-bearing dynamic. Coalition note: the largest payer group (mobile citizens) is dispersed across jurisdictions and franchise-less where it settles, which is precisely why its coalition potential stays unrealized despite its numbers.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute very differently. From the government seat the arrangement is a legitimate, democratically accountable coordination device it built and defends; from the trapped payer seats the same structure operates as enforced extraction with revocable status. Immigration bureaucracies straddle: they administer rules they did not originate while their budgets depend on the rules staying strict. Export-oriented employers experience the arrangement as misallocation rather than injustice — a pricing error they route around through arbitrage — which is why their powerful-but-excluded position produces lobbying rather than structural challenge. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are declared beneficiaries and hold the agenda-setter role: they derive low directionality (subsidized side) — the arrangement delivers them electoral credit and admission control at modest direct cost. Domestic labor-market incumbents are beneficiaries with constrained exit: wage and welfare-pool protection flows to them, damped directionality. Immigration bureaucracies benefit through budget and mandate growth and additionally administer the machinery — a dual position captured by secondary_role. Mobile citizens are victims with constrained exit: they bear compliance costs and revocability, amplified directionality. Cross-border workers and third-country nationals are victims with trapped exit: no alternative channel exists for them, placing them nearest the full-target end. Continental spatial scope on the payer seats amplifies effective extraction (verification of treatment across jurisdictions is hard); national scope on the setter seats keeps its verification comparatively cheap. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already produce the correct structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling pooled markets with national control of belonging — is live, so no mandatrophy is declared and none should be inferred. The tangled_rope classification prevents two symmetrical mislabelings: reading the arrangement as pure coordination (rope) ignores that the same structure that protects incumbent labor markets imposes revocability and compliance costs on people with no voice in setting them; reading it as pure extraction (snare) ignores the genuine coordination function — democratic accountability for admission, social-insurance sustainability, orderly rather than chaotic labor inflows — that real constituencies defend. The hybrid is held together by active enforcement, which is why requires_active_enforcement is declared. Watch item: if the conditionality omega resolves toward nominal (exit costs have made renegotiation illusory), the coordination half atrophies first and the arrangement drifts toward piton dynamics — maintained performatively by governments who could change it but bear less of its cost than the payers do.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the federation_membership kernel — the sovereignty_reading, under which membership is a conditional treaty and free movement is negotiable policy. What would the sibling integration_reading (membership irreversible, supranational authority legitimate, movement a constitutional right) change structurally?',
    'Compare seat classifications across the two files: the integration reading converts mobile citizens from payers into rights-holders, recasts states restricting movement as the violating party, and re-indexes epsilon for the same border practices as rights infringement rather than policy choice.',
    'If the integration reading were adopted as the operative framework, the identical enforcement apparatus classifies with a different victim set and different effective extraction per seat; epsilon here is reading-indexed and not comparable across readings without translation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: this story instantiates one reading of a contested kernel; the sibling reading would restructure beneficiary/victim sets entirely.').

omega_variable(
    membership_conditionality_credibility,
    'Is membership conditionality substantively credible — can states actually renegotiate or withdraw movement terms at acceptable cost — or has integration created reliance interests that render conditionality nominal?',
    'Observe actual renegotiation and withdrawal episodes: treaty amendment conferences, safeguard invocations, judicial treatment of withdrawal clauses, and realized exit costs for states that attempted them.',
    'If conditionality is nominal, the arrangement''s persistence rests on inertia rather than enforceable bargains, shifting classification toward piton dynamics and lowering effective extraction for the agenda-setting seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(membership_conditionality_credibility, empirical, 'Whether treaty conditionality is operative or increasingly theatrical.').

omega_variable(
    net_protection_gain_for_incumbents,
    'Do domestic labor-market incumbents gain net from mobility restriction, or do shortage, administrative, and fiscal costs exceed wage-protection gains for significant subsets of the declared beneficiary group?',
    'Regional labor-market studies comparing restricted and open periods and regions; distributional analysis of restriction costs by sector and skill level.',
    'If net-negative for large subsets, the beneficiary declaration overstates coordination support and the arrangement drifts toward extraction sustained by fewer, more concentrated beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_protection_gain_for_incumbents, empirical, 'Whether the declared beneficiary group actually nets positive from restriction.').

omega_variable(
    externalized_enforcement_visibility,
    'How much of the arrangement''s coercive enforcement is externalized to transit and origin countries through funding and cooperation agreements, and does domestically measured suppression therefore understate the total?',
    'Audit enforcement-cooperation treaties, funding flows to third-country border forces, and extraterritorial processing arrangements.',
    'If externalization is large, effective suppression exceeds the domestic measure and the extractive component of the hybrid is larger than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externalized_enforcement_visibility, empirical, 'Suppression may be understated when enforcement is offshored beyond the measurement perimeter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(fede_tr_t10, observed).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(fede_tr_t20, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(fede_tr_t30, observed).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(fede_tr_t40, observed).
narrative_ontology:measurement(fede_tr_t50, federation_membership__sovereignty_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement_basis(fede_tr_t50, observed).
narrative_ontology:measurement(fede_tr_t60, federation_membership__sovereignty_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(fede_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(fede_be_t10, observed).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(fede_be_t20, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(fede_be_t30, observed).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(fede_be_t40, observed).
narrative_ontology:measurement(fede_be_t50, federation_membership__sovereignty_reading, base_extractiveness, 50, 0.67).
narrative_ontology:measurement_basis(fede_be_t50, observed).
narrative_ontology:measurement(fede_be_t60, federation_membership__sovereignty_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(fede_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(fede_su_t10, observed).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(fede_su_t20, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(fede_su_t30, observed).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement_basis(fede_su_t40, observed).
narrative_ontology:measurement(fede_su_t50, federation_membership__sovereignty_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(fede_su_t50, observed).
narrative_ontology:measurement(fede_su_t60, federation_membership__sovereignty_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(fede_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the free movement debate' covers two structurally distinct constraints instantiated from one kernel, decomposed per the epsilon-invariance rule. This file authors the sovereignty_reading: epsilon's referent is the standing conditional-membership arrangement assessed by that reading's own lights (high extraction from mobility restriction borne by mobile citizens; border control treated as legitimate). The sibling file authors the integration_reading, under which the same border practices constitute rights infringement with a different victim set and a different epsilon. Coupling runs both ways: entrenched mobility jurisprudence on the integration side raises the cost of exercising this reading's conditionality, while every national derogation invoked under this reading pressures the sibling's legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
