% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the parliamentary sovereignty reading of the
 *   basic-law interpretive-authority kernel: the elected legislature, not the
 *   courts and not ongoing popular contestation, holds final say over
 *   constitutional meaning, exercised through legislative override of
 *   judicial rulings. The doctrine is framed as democratic accountability —
 *   the body answerable to voters should have the last word — but its
 *   structural effect is to make judicial protections contingent on
 *   legislative majorities, with rights-minorities and judicial institutional
 *   independence bearing the cost whenever override is exercised. This is a
 *   distinct constraint from the judicial_supremacy_reading (where courts
 *   hold final say and legislatures bear the cost of judicial error) and the
 *   popular_constitutionalism_reading (where no institution holds terminal
 *   authority and gridlock itself is the cost-bearer); each reading has its
 *   own epsilon and its own victim set and is authored as a separate file.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary and agenda-setter (institutional/arbitrage) — holds and exercises override authority
 *   - governing_majority_party: Concentrated beneficiary (powerful/mobile) — captures the override mechanism when in power
 *   - judicial_independence: Institutional payer (institutional/trapped) — authority eroded by successful overrides
 *   - rights_minorities: Primary target (powerless/trapped) — bears the direct cost of overridden protections
 *   - opposition_parties: Excluded when out of power (organized/constrained) — procedurally present, substantively powerless
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — documents override pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '1714861d-eaac-495d-9c46-853bd1046ad0').
narrative_ontology:cs_kernel_codification('1714861d-eaac-495d-9c46-853bd1046ad0', distributed).
narrative_ontology:cs_authority_grounding('1714861d-eaac-495d-9c46-853bd1046ad0', practice).
narrative_ontology:cs_interpretation_layer_present('1714861d-eaac-495d-9c46-853bd1046ad0').
narrative_ontology:cs_reading_relation('1714861d-eaac-495d-9c46-853bd1046ad0', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('1714861d-eaac-495d-9c46-853bd1046ad0', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('1714861d-eaac-495d-9c46-853bd1046ad0', foundational, electoral_accountability_confers_interpretive_legitimacy).
narrative_ontology:cs_axiom_status(electoral_accountability_confers_interpretive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1714861d-eaac-495d-9c46-853bd1046ad0', electoral_accountability_confers_interpretive_legitimacy, conventional).
narrative_ontology:cs_axiom('1714861d-eaac-495d-9c46-853bd1046ad0', secondary, unelected_judicial_finality_is_democratically_illegitimate).
narrative_ontology:cs_axiom_status(unelected_judicial_finality_is_democratically_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1714861d-eaac-495d-9c46-853bd1046ad0', unelected_judicial_finality_is_democratically_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('1714861d-eaac-495d-9c46-853bd1046ad0', parliamentary_supremacy_inheritance).
narrative_ontology:cs_drift_state('1714861d-eaac-495d-9c46-853bd1046ad0', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1714861d-eaac-495d-9c46-853bd1046ad0', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_party).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, democratic_mandate_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, representative_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on constitutional meaning by statute and, where the override mechanism exists, can supersede judicial rulings by ordinary or supermajority vote. Justifies this power by claiming direct electoral accountability that unelected judges lack. Retains the ability to change the rules of its own authority through further legislation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, beneficiary).

% Controls the legislative override mechanism whenever it holds a working majority. Benefits from the doctrine because it converts adverse court rulings into a political problem it can solve through ordinary legislative process rather than a legal one requiring judicial persuasion.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_party, beneficiary,
    powerful, biographical, mobile, national).

% The judiciary can rule on constitutional questions but any ruling adverse to the legislative majority can be overridden through statute, weakening the practical finality of judicial review. Courts have no exit from this structure — they can only issue opinions that carry persuasive but not terminal weight, and their institutional authority erodes each time an override succeeds.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence, payer,
    institutional, civilizational, trapped, national).

% Groups whose rights claims depend on judicial protection against majoritarian legislation bear the cost when the legislature overrides a favorable court ruling. They have no legislative majority to appeal to and no higher court to which to escalate once the override occurs; their protection is contingent on shifting electoral coalitions rather than fixed legal guarantee.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, biographical, trapped, national).

% Object to overrides when out of power but benefit from the same mechanism when they hold a majority. Currently without the votes to block overrides, they are procedurally present in debate but substantively unable to affect outcomes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parties, excluded,
    organized, biographical, constrained, national).

% Study the doctrine's operation, comparing override frequency and its effects on rights protection and judicial legitimacy across cases; take no side in individual disputes but document the pattern of majoritarian override outcomes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, electorally accountable tie-breaking mechanism for constitutional disagreement, avoiding indefinite deadlock between courts and elected representatives and locating final say with the body most directly answerable to voters.
% TRANSFER_FUNCTION: Moves ultimate interpretive authority from courts to the elected legislature, shifting the cost of erroneous or majoritarian rights-infringing interpretations from the legislature (which faces electoral consequences only diffusely and periodically) onto minorities and the judiciary's institutional standing whenever an override occurs.
% ABSENT_VOICES: Rights-minorities affected by specific overrides are rarely organized as a bloc capable of contesting the doctrine itself; they experience individual override outcomes without a forum to challenge the interpretive-authority allocation that produced them. Future minorities not yet identified are entirely absent from the current debate.
% DISAPPEARANCE_RATIONALE: If legislative override authority disappeared, judicial rulings would become terminal, rights-protective jurisprudence would harden into fixed precedent immune to majoritarian correction, and the legislature would lose its principal tool for resisting what it views as judicial overreach — the balance of constitutional power between the two branches would shift decisively toward courts.
% FOUNDING_PROBLEM: Historical distrust of unelected judiciaries making final, unreviewable constitutional determinations without democratic input, combined with a desire to preserve legislative supremacy inherited from parliamentary tradition.
% FOUNDING_PROBLEM_CORROBORATION: Legislators and governing-party constitutional theorists attest the problem remains live, citing ongoing instances of judicial rulings seen as countermajoritarian overreach. Independent constitutional scholars and civil liberties organizations outside the legislative majority attest that override use has increasingly targeted rights-protective rulings for narrow political majorities rather than genuine democratic-accountability concerns, suggesting the doctrine's operation has drifted from its founding rationale.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52) because the mechanism does not extract continuously — it activates only when override is exercised — but each exercise transfers a concrete cost (a lost rights protection, a diminished judicial ruling) from the legislature to minorities and courts. Suppression (0.58) reflects that once an override succeeds, there is no further appeal within the domestic system; the alternative (judicial finality) is foreclosed for that specific ruling. Theater ratio is modest (0.28) — the accountability rationale is genuine in that legislators do face elections, but the rationale increasingly covers narrow majoritarian corrections rather than broad-based democratic deliberation, which the rising measurement series tracks.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature and the governing majority party sit near the beneficiary end: they set the rule, exercise it, and bear no direct cost from its use (electoral accountability is diffuse and delayed, not a real-time check on any specific override). Judicial independence and rights-minorities sit near the target end: they cannot exit the jurisdiction, cannot appeal beyond the override, and bear the concrete consequence each time the mechanism is used. Opposition parties are structurally ambivalent — beneficiaries when in power, victims when out — but currently excluded from effective influence, which is why they are marked excluded rather than payer or beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resisting an unaccountable judiciary — remains partially live (courts are not directly elected), which prevents an unqualified 'dead mandate' classification; but the corroboration record shows the doctrine's actual use has shifted toward overriding rights-protective rulings for narrow partisan advantage rather than resolving genuine democratic-accountability deficits. This mismatch (founding_problem_status: contested, disappearance_verdict: world_rearranges) flags a partial-capture pattern worth tracking rather than either full mandatrophy or full continued legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    override_frequency_vs_genuine_correction,
    'Does the legislative override mechanism, in actual practice, correct genuine judicial overreach, or does it primarily reverse rights-protective rulings unfavorable to the current majority?',
    'Empirical audit of override instances over the interval: classify each by whether the overridden ruling protected a minority right against majoritarian legislation versus corrected a genuine judicial error of constitutional interpretation unrelated to majority/minority conflict.',
    'If overrides skew heavily toward reversing minority-protective rulings, the doctrine''s coordination framing (correcting judicial error) is substantially cover for extraction (removing inconvenient rights protections), supporting reclassification toward snare; if overrides are evenly distributed across genuine interpretive disputes, the tangled_rope classification with real coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_frequency_vs_genuine_correction, empirical, 'Whether override use tracks genuine correction or majoritarian rights-reversal.').

omega_variable(
    kernel_committer_structure,
    'This constraint is one reading (parliamentary_sovereignty_reading) of the basic_law_interpretive_authority kernel. The sibling readings (judicial_supremacy_reading, popular_constitutionalism_reading) would locate final authority differently and produce different victim sets — what determines which reading a given constitutional order actually instantiates, and can more than one reading operate simultaneously within different domains of the same legal system?',
    'Comparative institutional analysis: examine whether the jurisdiction''s override mechanism is used consistently across all constitutional domains (supporting a clean parliamentary_sovereignty reading) or only in some domains while courts retain practical finality elsewhere (supporting a hybrid or popular_constitutionalism reading in practice).',
    'If the jurisdiction operates a genuinely mixed system, this story''s epsilon and victim set apply only to the domains where override is actually exercised, and a companion story should be written for domains where judicial finality holds in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_structure, conceptual, 'Committer structure: this is one reading of a contested kernel; sibling readings are separate constraints with different victim sets and epsilon values, not alternative measurements of this one.').

omega_variable(
    electoral_accountability_realism,
    'Is the electoral accountability that justifies legislative interpretive supremacy a real, operative check, or a formal fiction given low-salience voting on constitutional-override specifics?',
    'Survey and electoral-behavior analysis: do voters meaningfully punish legislators for specific override votes, or are override votes invisible within broader partisan voting patterns?',
    'If accountability is largely fictional, the doctrine''s core legitimacy claim (democratic mandate) is substantially theatrical, which would push the theater_ratio and extractiveness assessment higher over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_accountability_realism, empirical, 'Whether electoral accountability for override decisions is a real check or largely nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the basic_law_interpretive_authority kernel, each authored as a separate constraint story with its own epsilon and structural data per the ε-invariance principle. The judicial_supremacy_reading places final authority with courts (victims: legislative majorities whose enactments are struck down); the popular_constitutionalism_reading denies terminal authority to any institution (victims: parties seeking legal certainty, who bear ongoing contestation costs). This reading (parliamentary_sovereignty_reading) places final authority with the legislature (victims: judicial independence and rights-minorities). The three readings are linked via affects_constraints but are not merged, averaged, or treated as measurements of one underlying constraint — they are three distinct constraints sharing a contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
