% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__developmental_potentiality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__developmental_potentiality_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: legal_personhood_boundary__developmental_potentiality_reading
 *   human_readable: Personhood-at-Conception Doctrine (Developmental Potentiality Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates the developmental-potentiality reading of
 *   the contested legal-personhood-boundary kernel: personhood attaches at
 *   conception because the continuous biological trajectory from zygote to
 *   born human admits no other principled discontinuity. Under this reading,
 *   the fetus enters the rights-bearing class from conception, the pregnant
 *   person's bodily autonomy is legally subordinated wherever it conflicts
 *   with fetal interest, and the state acquires expanded enforcement
 *   jurisdiction over pregnancy outcomes — investigating miscarriages,
 *   prosecuting substance use during pregnancy, and asserting fetal
 *   wrongful-death claims. This is a single reading among three sibling
 *   readings of the same kernel (functional_capacity_reading and
 *   restrictive_anthropocentric_reading, generated as separate constraint
 *   stories); this story does not describe or average across those readings —
 *   it authors one stable epsilon for the arrangement as this reading's own
 *   advocates and its critics, by the reading's own lights, describe its
 *   actual operation.
 *
 * KEY AGENTS:
 *   - pregnant_people: Primary target (powerless/trapped) — bears subordination of autonomy and criminal/civil liability exposure
 *   - low_income_pregnant_people: Most exposed target (powerless/trapped) — bears disproportionate enforcement burden with least exit capacity
 *   - medical_providers_of_reproductive_care: Secondary target (moderate/constrained) — bears liability exposure and narrowed practice scope
 *   - state_prosecutorial_authorities: Primary agenda-setter (institutional/analytical) — administers and enforces the standard, gains jurisdiction
 *   - fetal_rights_advocacy_organizations: Primary beneficiary (organized/arbitrage) — gains institutional standing and durability without enforcement cost
 *   - religious_institutions_endorsing_conception_doctrine: Secondary beneficiary (institutional/arbitrage) — doctrinal commitment becomes enforceable civil law
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicates the boundary dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__developmental_potentiality_reading, 0.79).
domain_priors:theater_ratio(legal_personhood_boundary__developmental_potentiality_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legal_personhood_boundary__developmental_potentiality_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__developmental_potentiality_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__developmental_potentiality_reading, "Personhood-at-Conception Doctrine (Developmental Potentiality Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__developmental_potentiality_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__developmental_potentiality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__developmental_potentiality_reading, 'a535e90c-2c9c-4bd2-aaf1-aaac7df7d867').
narrative_ontology:cs_kernel_codification('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', distributed).
narrative_ontology:cs_authority_grounding('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', distributed).
narrative_ontology:cs_reading_relation('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', foundational, continuous_biological_trajectory_grounds_rights).
narrative_ontology:cs_axiom_status(continuous_biological_trajectory_grounds_rights, holdable).
narrative_ontology:cs_axiom_grounding('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', continuous_biological_trajectory_grounds_rights, deontological).
narrative_ontology:cs_axiom('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', secondary, developmental_potential_sufficient_for_moral_status).
narrative_ontology:cs_axiom_status(developmental_potential_sufficient_for_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', developmental_potential_sufficient_for_moral_status, deontological).
narrative_ontology:cs_reference_frame('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', continuous_developmental_trajectory_standard).
narrative_ontology:cs_drift_state('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', post_dobbs_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a535e90c-2c9c-4bd2-aaf1-aaac7df7d867', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__developmental_potentiality_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, state_prosecutorial_authorities).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__developmental_potentiality_reading, religious_institutions_endorsing_conception_doctrine).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, pregnant_people).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, medical_providers_of_reproductive_care).
narrative_ontology:constraint_victim(legal_personhood_boundary__developmental_potentiality_reading, low_income_pregnant_people).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, continuous_human_life_trajectory_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__developmental_potentiality_reading, sanctity_of_developmental_potential).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the direct legal and physical consequences of a rule that treats the zygote/embryo/fetus as a rights-bearer from conception: their own bodily autonomy, medical decision-making, and legal agency during pregnancy become subordinate to the fetal life trajectory once conception is legally established. Exit is essentially unavailable once pregnant in a jurisdiction that enforces this reading; travel to another jurisdiction is possible for some but not for the poor, the detained, or those under active state monitoring.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, pregnant_people, payer,
    powerless, immediate, trapped, national).

% Face civil and criminal liability for actions that could be construed as harming the fetal rights-holder, including standard obstetric interventions in emergencies where fetal and maternal interests may conflict. Some relocate practice to more permissive jurisdictions; many simply narrow the range of care they are willing to provide, which constrains their professional exit to leaving the specialty altogether.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, medical_providers_of_reproductive_care, payer,
    moderate, biographical, constrained, national).

% Carry the enforcement burden disproportionately: cannot travel to more permissive jurisdictions for care, face the most direct surveillance from state agencies (prenatal drug testing, mandatory reporting), and are most likely to face prosecution for miscarriage, stillbirth, or substance use during pregnancy under fetal-personhood statutes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, low_income_pregnant_people, payer,
    powerless, immediate, trapped, regional).

% Administer and enforce the personhood-at-conception standard through statute, charging decisions, and civil commitment or reporting mandates. Gain expanded jurisdiction over pregnancy outcomes — miscarriage investigations, feticide statutes, wrongful death claims on behalf of a fetus — that did not exist under a birth-line standard.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, state_prosecutorial_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Built organizational, legal, and fundraising infrastructure around establishing and defending conception as the personhood threshold. Gain legal standing, political influence, and institutional durability from the doctrine's adoption and defense in courts and legislatures, without bearing any of the enforcement costs themselves.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, fetal_rights_advocacy_organizations, beneficiary,
    organized, generational, arbitrage, national).

% See a core doctrinal commitment enacted into enforceable civil law, which extends institutional moral authority into the domain of state power without the institution itself bearing the costs of enforcement, litigation, or medical liability.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, religious_institutions_endorsing_conception_doctrine, beneficiary,
    institutional, civilizational, arbitrage, national).

% Not represented as parties despite arguments the doctrine's harms (or benefits) fall on the next generation through effects on maternal healthcare access and family economic stability; the debate proceeds without a mechanism for their interests to be independently assessed.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, future_born_children, excluded,
    powerless, generational, analytical, national).

% Adjudicate disputes over where the personhood line sits and what enforcement powers follow from it, drawing on developmental biology, constitutional text, and precedent from other rights-boundary disputes.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__developmental_potentiality_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__developmental_potentiality_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__developmental_potentiality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively bright-line rule — conception — for determining when legal rights-bearing status begins, replacing the need for case-by-case biological or developmental judgment calls in criminal law, tort law, immigration, inheritance, and welfare eligibility.
% TRANSFER_FUNCTION: Moves decisional authority over pregnancy outcomes from the pregnant person to the state and to fetal-interest representatives (prosecutors, courts, guardians ad litem), and moves legal, medical, and reputational risk from potential fetal harm onto pregnant people and their medical providers.
% ABSENT_VOICES: The zygote/embryo/fetus itself cannot articulate a position; the doctrine's advocates speak on its behalf without independent verification of what interest-bearing status would mean at each developmental stage. Low-income pregnant people bear the enforcement weight but have the least access to litigate or lobby against it. Medical ethicists proposing graduated or interest-based standards are frequently excluded from statutory drafting processes dominated by advocacy coalitions.
% DISAPPEARANCE_RATIONALE: If the conception-personhood standard were repealed, pregnancy-related prosecutions for miscarriage and substance use would collapse, physician liability exposure in obstetric emergencies would sharply narrow, wrongful-death-of-fetus civil claims would disappear, and state agencies currently tasked with monitoring pregnancy outcomes would lose that jurisdiction — a substantial rearrangement of criminal law, tort law, and reproductive healthcare practice.
% FOUNDING_PROBLEM: Advocates state the doctrine was built to resolve a genuine philosophical and biological ambiguity: there is no sharp biological discontinuity between zygote and infant, so any line drawn after conception is claimed to be arbitrary, and continuous human development is offered as the only principled place to anchor rights.
% FOUNDING_PROBLEM_CORROBORATION: Fetal rights organizations and allied legislators attest the founding problem (a nonarbitrary rights threshold) remains live and unsolved by any other reading. Independent bioethicists, maternal-fetal medicine associations, and civil liberties organizations outside the advocacy coalition attest that the practical function has shifted from resolving a philosophical ambiguity to expanding prosecutorial jurisdiction over pregnancy, citing the pattern of miscarriage and stillbirth prosecutions that the founding argument does not by itself predict or require.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__developmental_potentiality_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__developmental_potentiality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__developmental_potentiality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__developmental_potentiality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__developmental_potentiality_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__developmental_potentiality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__developmental_potentiality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 by interval end: the doctrine's coordination claim (a nonarbitrary rights threshold) is real but thin relative to the concrete transfer it enacts — decisional authority and legal risk move from pregnant people to the state and to fetal-interest representatives, and that transfer has widened over the measured interval as feticide and wrongful-death statutes have proliferated and prosecutions for pregnancy outcomes have increased. Suppression is authored higher (0.79) than extraction because the constraint's persistence depends on active enforcement machinery — criminal statutes, mandatory reporting, civil commitment mechanisms — not on voluntary participant buy-in; a pregnant person cannot simply decline the doctrine's application to her body once resident in an enforcing jurisdiction. Accessibility collapse is moderate (0.5) rather than high because interstate and international travel remains a partial alternative for some, which is exactly why the doctrine's harms concentrate so sharply on those without travel capacity. Resistance is high (0.81), reflecting the doctrine's status as one of the most actively and publicly contested legal claims in the jurisdiction, litigated at every level of the court system.
 *
 * DIRECTIONALITY LOGIC:
 *   Fetal rights advocacy organizations and allied religious institutions sit at the beneficiary end: they gain durable legal and political standing from the doctrine's adoption without bearing its enforcement costs (arbitrage exit — they can relocate advocacy resources across jurisdictions freely). State prosecutorial authorities are the agenda-setter: they gain expanded jurisdiction and administer the enforcement machinery, distinct from being a beneficiary in the rent-collecting sense. Pregnant people, and especially low-income pregnant people, sit at the full-target end: their exit options are trapped, their exposure is immediate and physical, and the doctrine's costs land on them with the least capacity to avoid it. Medical providers occupy an intermediate position — moderate power, constrained exit — bearing professional and legal risk without being the doctrine's primary intended target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview captures the doctrine's genuine philosophical claim (no principled discontinuity exists between zygote and infant) while separately tracking whether that claim, whatever its philosophical merit, now functions primarily as jurisdictional expansion for state prosecutorial power over pregnancy outcomes. The status is authored as contested rather than resolved in either direction: this reading's own advocates maintain the founding problem is live and unsolved by rival readings, while independent maternal-fetal medicine associations and civil liberties organizations outside the beneficiary coalition attest that the doctrine's practical operation — increasing miscarriage and stillbirth prosecutions — outstrips what the philosophical argument alone would predict or require. This divergence between the doctrine's stated justification and its measured enforcement pattern is exactly the kind of drift the classification is built to surface rather than paper over.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_normative_vs_structural,
    'Is the choice among the three personhood-boundary readings (conception, birth, functional capacity) a resolvable empirical/biological question, or an irreducibly normative choice about which criterion (biological continuity, birth event, cognitive capacity) counts as morally decisive?',
    'No empirical resolution mechanism exists: developmental biology can describe the continuous trajectory from zygote to infant without settling which point on that trajectory triggers rights-bearing status, since that is a normative rather than a biological question. Resolution would require either broad cross-jurisdictional legal-philosophical consensus (currently absent) or a constitutional-level settlement that this framework does not adjudicate.',
    'If irreducibly normative, no single reading can claim to be ''the correct'' personhood boundary without begging the question — all three readings remain simultaneously live, and enforcement of any one over the others is a political and coercive fact, not a discovered truth. This bears directly on how much weight the doctrine''s own coordination-function claim (a nonarbitrary threshold) can bear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_normative_vs_structural, conceptual, 'Whether the personhood-boundary kernel has a determinable answer or is an irreducible normative choice among the three sibling readings.').

omega_variable(
    founding_problem_vs_jurisdictional_expansion,
    'Does the conception-personhood doctrine''s actual enforcement pattern (miscarriage and stillbirth prosecutions, expanded fetal wrongful-death claims) track the philosophical founding problem it claims to solve, or has enforcement decoupled from the founding rationale into an independent jurisdictional-expansion project?',
    'Comparative analysis of prosecution patterns and statutory drafting history across enforcing jurisdictions: if new enforcement actions consistently track cases where the philosophical rationale would predict fetal-interest vindication (e.g., third-party violence against pregnant people), decoupling is weak; if enforcement concentrates disproportionately on the pregnant person''s own conduct (substance use, delayed care-seeking, self-managed miscarriage), decoupling is substantial.',
    'Strong decoupling would support reclassifying the doctrine''s operative function as closer to a snare (extraction from pregnant people under coordination cover) than a tangled rope (genuine if asymmetric coordination); weak decoupling would support the tangled_rope classification as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_vs_jurisdictional_expansion, empirical, 'Whether enforcement of the conception standard tracks its stated philosophical justification or has become an independent extraction mechanism.').

omega_variable(
    fetal_interest_representation_validity,
    'Can any third party (prosecutor, guardian ad litem, advocacy organization) validly represent a fetal rights-holder''s interests given the fetus''s total inability to articulate, contest, or ratify that representation?',
    'No empirical resolution exists; this is a structural feature of any personhood standard applied to an entity incapable of communication, and is shared to some degree by other non-communicative rights-holders (infants, severely incapacitated adults) who have established guardian ad litem frameworks — the question is whether those frameworks transfer coherently to the prenatal context or whether the analogy breaks down.',
    'If representation is structurally invalid or radically underdetermined in this context, the doctrine''s coordination claim (resolving a rights question) collapses into pure assertion by whichever party controls the representational apparatus (typically the state or advocacy organizations), strengthening the extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fetal_interest_representation_validity, conceptual, 'Whether third-party representation of fetal interests is coherent or merely a vehicle for other parties'' agendas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__developmental_potentiality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__developmental_potentiality_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__developmental_potentiality_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__developmental_potentiality_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__developmental_potentiality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__developmental_potentiality_reading, functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the legal_personhood_boundary kernel. developmental_potentiality_reading (this story) places the rights threshold at conception; restrictive_anthropocentric_reading places it at birth with demonstrated cognitive capacity; functional_capacity_reading places it at demonstrated cognitive capacity regardless of species or developmental stage. Each reading is authored as its own constraint with its own epsilon, victim set, and classification per the epsilon-invariance principle — this story's authored extraction (0.68, tangled_rope) describes only the conception-threshold arrangement's operation as its own advocates and critics characterize it, and must not be averaged with or substituted for the siblings' values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
