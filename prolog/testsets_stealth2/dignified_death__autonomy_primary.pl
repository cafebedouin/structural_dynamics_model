% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Death-Timing Authority Gate (Autonomy-Primary Reading)
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the legal-medical regime
 *   governing the timing and method of death: criminal prohibition of
 *   assisted dying in most jurisdictions, and where access exists, a
 *   gatekeeping apparatus of physician authorization, eligibility criteria,
 *   waiting periods, and review commissions that subordinates the individual
 *   request to institutional approval. This story instantiates the
 *   autonomy_primary reading of the dignified_death kernel, under which
 *   dignity resides in self-determination and the competent sufferer's
 *   considered request carries final authority; epsilon is therefore authored
 *   for the standing arrangement AS THIS READING SEES IT, not for the
 *   individual-sovereign regime the reading would endorse. Per the
 *   epsilon-invariance principle the colloquial label 'dignified death'
 *   decomposes into three structurally distinct constraints with different
 *   victim sets and epsilon values; the siblings (sanctity_primary,
 *   relational_autonomy) are separate stories linked through
 *   network.affects_constraints. The claim/metric relationship is deliberate:
 *   the arrangement presents itself as protection and medical integrity
 *   (rope-shaped), while the authored metrics describe a substantially
 *   extractive, actively enforced gate whose costs fall on identifiable
 *   denied classes.
 *
 * KEY AGENTS:
 *   - competent_suffering_individuals: Primary target (powerless/trapped) - bears prolonged suffering as the price of the protective gate
 *   - ineligible_chronic_suffering_patients: Secondary target (powerless/constrained) - excluded by the eligibility line even where access exists
 *   - medical_profession_gatekeepers: Agenda-setter and collector (institutional/mobile) - administers eligibility, retains authority over death, identity-fused with the healing ethic
 *   - legislative_and_prosecutorial_authorities: Agenda-setter (institutional/arbitrage) - sets and enforces the criminal and regulatory frame, experiments across jurisdictions
 *   - coercion_vulnerable_patients: Intended beneficiary (powerless/constrained) - shielded from familial and economic pressure by the gate
 *   - advanced_dementia_patients: Excluded seat (powerless/trapped) - loses capacity before any request window opens
 *   - bioethics_commissions_and_courts: Analytical observer (institutional/analytical) - redraws the gate through rulings and reports
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.55).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.62).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.55).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Death-Timing Authority Gate (Autonomy-Primary Reading)").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '82a12e1c-6ee7-4072-8cc4-8435274eb29a').
narrative_ontology:cs_kernel_codification('82a12e1c-6ee7-4072-8cc4-8435274eb29a', distributed).
narrative_ontology:cs_authority_grounding('82a12e1c-6ee7-4072-8cc4-8435274eb29a', distributed).
narrative_ontology:cs_reading_relation('82a12e1c-6ee7-4072-8cc4-8435274eb29a', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('82a12e1c-6ee7-4072-8cc4-8435274eb29a', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('82a12e1c-6ee7-4072-8cc4-8435274eb29a', foundational, consent_confers_final_authority_over_death).
narrative_ontology:cs_axiom_status(consent_confers_final_authority_over_death, holdable).
narrative_ontology:cs_axiom_grounding('82a12e1c-6ee7-4072-8cc4-8435274eb29a', consent_confers_final_authority_over_death, deontological).
narrative_ontology:cs_axiom('82a12e1c-6ee7-4072-8cc4-8435274eb29a', secondary, prolonged_denial_of_exit_is_dignity_harm).
narrative_ontology:cs_axiom_status(prolonged_denial_of_exit_is_dignity_harm, holdable).
narrative_ontology:cs_axiom_grounding('82a12e1c-6ee7-4072-8cc4-8435274eb29a', prolonged_denial_of_exit_is_dignity_harm, deontological).
narrative_ontology:cs_reference_frame('82a12e1c-6ee7-4072-8cc4-8435274eb29a', individual_self_determination_supremacy).
narrative_ontology:cs_drift_state('82a12e1c-6ee7-4072-8cc4-8435274eb29a', contemporary_regulated_access_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('82a12e1c-6ee7-4072-8cc4-8435274eb29a', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, coercion_vulnerable_patients).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_profession_gatekeepers).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, competent_suffering_individuals).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, ineligible_chronic_suffering_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with decision-making capacity enduring incurable suffering who want to determine the timing and manner of their own death. The standing arrangement answers their request with prohibition or with a gate they may not pass: criminal exposure for anyone who assists them, physician refusal backed by licensing discipline, or eligibility criteria they fail. Their alternatives are expensive travel to permissive jurisdictions, palliative sedation that does not return authority over timing, or violent self-deliverance. Their organizing window closes as their condition advances, so collective resistance is structurally difficult.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, competent_suffering_individuals, payer,
    powerless, immediate, trapped, national).

% Patients whose suffering is severe and durable but who fail the eligibility line: non-terminal chronic illness, psychiatric suffering, disability without terminal prognosis. Even in jurisdictions that permit assisted dying for the terminal, they are told the gate exists but not for them. Some can afford exit abroad; most cannot. They bear the denial indefinitely rather than terminally.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, ineligible_chronic_suffering_patients, payer,
    powerless, biographical, constrained, national).

% Physicians, medical boards, and hospital ethics structures that administer the gate: certify eligibility, sign authorizations, run waiting periods and review referrals, and discipline members who act outside the frame. The arrangement concentrates legitimate authority over death in their hands and shields them from liability for both acting and refusing. Their professional identity is bound to the healing-not-killing ethic the arrangement vindicates; they can relocate practice across jurisdictions but not out of the professional identity itself.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_profession_gatekeepers, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_profession_gatekeepers, beneficiary).

% Legislatures, ministries, and prosecutors who set and enforce the criminal and regulatory frame: which assistance is a crime, which exceptions exist, what reporting and review the permitted channels require. They avoid the administrative and moral cost of operating a death-request system by leaving the gate closed or narrow, and they can experiment jurisdiction-by-jurisdiction, importing or refusing neighboring reforms at low cost to themselves.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, legislative_and_prosecutorial_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Elderly, dependent, or cognitively fragile patients who would be exposed to pressure from heirs, exhausted caregivers, or cost-conscious institutions if death-on-request were freely available. The gate exists substantially for them: mandatory waiting periods, independent witnesses, and physician-only authorization raise the cost of steering a relative toward death. They receive this protection whether or not they ever want it.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, coercion_vulnerable_patients, beneficiary,
    powerless, biographical, constrained, national).

% People progressing toward incapacity who will lose the capacity to request anything before most eligibility windows open. Advance directives are thinly honored for death timing, and the entire request-based architecture presupposes a competent asking agent they will not be at the relevant moment. They have the most at stake in the rule design and the least ability to be present in it.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, advanced_dementia_patients, excluded,
    powerless, biographical, trapped, national).

% Royal commissions, parliamentary committees, constitutional and appellate courts that hear testimony from the other seats, weigh autonomy claims against protection claims, and issue rulings or reports that redraw the gate. They decide nothing for themselves and collect nothing; their outputs reshape which requests pass.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, bioethics_commissions_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, medical_profession_gatekeepers).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The gate solves a real collective problem in an irreversibly coercible domain: death decisions cannot be appealed by the decedent, so a centralized channel with medical certification, witnesses, waiting periods, and review makes coerced or abusive death harder, keeps deaths documented and auditable rather than private, and preserves a single accountable locus for an act society otherwise cannot police.
% TRANSFER_FUNCTION: Moves final authority over death timing from the suffering individual to the state-medical gatekeeping apparatus; moves the risk of premature or steered death away from the vulnerable and onto the competent sufferer, who pays for the protective gate in prolonged suffering; moves the authorizing labor and liability to physicians and review bodies.
% ABSENT_VOICES: Advanced dementia patients are structurally voiceless at decision time and thinly represented in rule design. Disability advocates object that eligibility expansion under cost pressure converts autonomy into steerage; their testimony is heard in hearings but is not operative in criteria design. Impoverished patients whose 'choice' is conditioned on whether care remains affordable rarely appear as a distinct seat at all.
% DISAPPEARANCE_RATIONALE: Overnight removal of the prohibition-and-gatekeeping arrangement would immediately rearrange end-of-life practice: assistance would move from criminal courts to clinical settings or open markets, physicians would split between providing and refusing, insurers and health systems would face immediate steerage-risk questions they currently defer to the gate, and every jurisdiction would need replacement rules within months rather than decades.
% FOUNDING_PROBLEM: The arrangement was built to solve a triple problem: protect vulnerable people from being pressed toward death by interested parties, preserve medicine's healing mandate against its conversion into killing, and prevent abuse in a domain where the affected person cannot later complain or testify.
% FOUNDING_PROBLEM_CORROBORATION: The vulnerability problem is corroborated from outside the benefiting parties: hospice and palliative-care literature documents inheritance-motivated and caregiver-exhaustion pressure cases, and elder-abuse research substantiates coercibility. Disability rights organizations corroborate the vulnerability concern while disputing that blanket prohibition is the remedy. Court records from the major constitutional challenges record both the protection evidence and the autonomy evidence. No party outside the gatekeeping establishment attests that the founding problem justifies the arrangement's present breadth.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55, inside the 0.45-0.60 band the reading assigns to state prohibition: the referent is the standing arrangement, and from the autonomy-primary seat its core operation is denying exit to competent sufferers and charging ineligible classes for a gate built around them. Suppression is 0.62 and is a raw structural property, unscaled by power or scope: criminal exposure for assisters, licensing discipline for physicians, and travel-cost filtering of alternatives. Theater ratio is 0.35: waiting periods, second opinions, and commission reviews are partly functional (they catch coercion and error) and partly delaying ritual that exhausts applicants near death. Accessibility collapse is 0.45, not mountain-grade: alternatives persist (Swiss exit organizations, palliative sedation, treatment refusal, violent self-help) but are costly, exclusionary, or categorically unlike what was requested. Resistance is 0.70: sustained constitutional litigation, ballot initiatives, high and stable public support, and organized civil disobedience. A structural note on coalition failure: the primary target class cannot easily form coalitions because each member's window closes with their disease; the constraint extracts precisely from people whose capacity for collective action is dying with them. The measurement series run on one shared time grid (all three metrics at all seven points) so no metric row borrows another's end-state values. The enforcement story is a genuine capacity transformation, hence the suppression_requirement series: blunt, low-capacity criminal enforcement gave way after 2002 to dense continuous regulatory enforcement (reporting systems, audits, physician discipline), peaking mid-interval and easing slightly as permissive regimes normalized.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute differently. From the competent sufferer's position the arrangement is close to a snare: a locked exit defended by criminal law, with the coordination story experienced as the lock's alibi. From the gatekeeper's position it is the profession's fiduciary structure: the same gate that denies them is what makes their authorization meaningful and their refusals safe. The gatekeeper seat additionally carries an identity-lock mechanism of the institutional kind: the organization has become its function, and 'medicine heals, does not kill' is constitutive rather than instrumental; if that identity frame broke, the gatekeeping would read as usurpation of patient sovereignty and the seat's computed classification would shift sharply toward extraction-collaboration. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent suffering individuals and ineligible chronic patients are declared victims with trapped-to-constrained exit, placing them near the full-target end of directionality; effective extraction is amplified for them, most extremely for the trapped terminal class. Coercion-vulnerable patients are declared beneficiaries with no arbitrage exit, placing them near the beneficiary end where the gate operates as subsidy (protection delivered at others' expense). Medical profession gatekeepers are dual-positioned: they administer the gate (agenda-setter) and collect from it (retained authority, liability shield), so their derived directionality sits well below symmetric despite their enforcement role. Legislative authorities are not declared in the beneficiary or victim arrays; they take the canonical fallback for their power atom, which is acceptable here since their position (avoided administrative cost, jurisdictional arbitrage) is mild-beneficiary rather than load-bearing for the classification. Spatial scopes are national for the affected classes and continental for the profession, reflecting where verification of the gate's operation actually happens.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both errors visible. Reading the arrangement as pure snare erases the real protection it delivers to the coercible - a benefit with documented corroborating literature and a live underlying problem. Reading it as pure rope erases the identified paying classes whose suffering is the gate's operating cost. Mandatrophy is checked rather than assumed: the founding problem (protect the coercible, preserve medicine's mandate) is contested rather than dead, so this is not a zombie institution maintained by inertia; the theater ratio of 0.35 sits below the piton threshold and the gate's function is demonstrably alive. The piton signature fails for a further reason: a concentrated collector exists (the gatekeeping profession), which is snare-flavored capture, not administrator-without-stake decay. The R5 mismatch consumer finds status=contested paired with verdict=world_rearranges, so no capture/zombie flag fires; the arrangement persists because the problem is disputed, not because everyone has forgotten what it was for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the autonomy_primary reading of the dignified_death kernel; how would the victim set, beneficiary structure, and epsilon shift under the sanctity_primary and relational_autonomy sibling readings?',
    'Generate the sibling stories against the same standing arrangement and compare the Phase B derivations; the structural delta between the three files locates the disagreement precisely.',
    'Under sanctity_primary the permissive-access features of the standing arrangement become the violation side and epsilon redistributes across regimes; under relational_autonomy agenda-setter status spreads to the triad and the individual victim set shrinks to overridden-patient cases. Classification of THIS file is stable only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the dignified_death kernel; sibling deltas routed here rather than folded into the constraint.').

omega_variable(
    consent_authenticity_under_cost_pressure,
    'Are requested exits autonomous when care is costly, caregivers are exhausted, and heirs are attentive - or are some requests internalized adaptations to feeling like a burden?',
    'Compare request rates and stated motives across care-funding regimes (generous home-care versus austerity jurisdictions), and motive studies of assisted-death applicants distinguishing pain-driven from burden-driven requests.',
    'If burden-motivated requests dominate, effective autonomy is lower than the reading assumes, the protective gate gains justification, and epsilon for the standing arrangement falls; if pain-motivated requests dominate under generous funding, denial approaches pure extraction and epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_authenticity_under_cost_pressure, empirical, 'Whether the autonomy the reading defends survives contact with economic and relational pressure.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the denial-of-exit suppression borne by suffering individuals wholly structural (criminal law, gatekeeping, travel cost) or partly internalized (acceptance of the prohibition as morally required)?',
    'Post-decriminalization demand trajectory: if demand for assisted death surges where legal access arrives, prior abstention was structural; if demand stays flat, part of the suppression was carried inside the targets.',
    'Internalized suppression raises true suppression above the structural measure and shifts part of the victim harm from imposed to absorbed; structural confirmation supports the high-chi reading for trapped targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism of the suppression metric in an end-of-life context.').

omega_variable(
    eligibility_boundary_mobility,
    'Does the eligibility line (terminal-only, chronic track, psychiatric inclusion) mark a principled boundary between protection and extraction, or an administrable compromise that migrates under advocacy and fiscal pressure?',
    'Track jurisdictional expansions (second-track chronic access, mental-illness scheduling, psychiatric practice in permissive regimes) against abuse and incident data; flat incident rates under expansion indicate the boundary was administrative rather than principled.',
    'If administrative, the epsilon authored here understates extraction for the excluded classes and the tangled_rope balance shifts toward snare for the ineligible seat; if principled, part of the measured extraction is the irreducible price of the protective function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_mobility, conceptual, 'Stability of the eligibility line that separates the gate''s coordination function from its extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1990, dignified_death__autonomy_primary, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(dign_tr_t1997, dignified_death__autonomy_primary, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(dign_tr_t2002, dignified_death__autonomy_primary, theater_ratio, 2002, 0.24).
narrative_ontology:measurement(dign_tr_t2009, dignified_death__autonomy_primary, theater_ratio, 2009, 0.28).
narrative_ontology:measurement(dign_tr_t2015, dignified_death__autonomy_primary, theater_ratio, 2015, 0.31).
narrative_ontology:measurement(dign_tr_t2021, dignified_death__autonomy_primary, theater_ratio, 2021, 0.33).
narrative_ontology:measurement(dign_tr_t2025, dignified_death__autonomy_primary, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(dign_be_t1990, dignified_death__autonomy_primary, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(dign_be_t1997, dignified_death__autonomy_primary, base_extractiveness, 1997, 0.59).
narrative_ontology:measurement(dign_be_t2002, dignified_death__autonomy_primary, base_extractiveness, 2002, 0.57).
narrative_ontology:measurement(dign_be_t2009, dignified_death__autonomy_primary, base_extractiveness, 2009, 0.56).
narrative_ontology:measurement(dign_be_t2015, dignified_death__autonomy_primary, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(dign_be_t2021, dignified_death__autonomy_primary, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(dign_be_t2025, dignified_death__autonomy_primary, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1990, dignified_death__autonomy_primary, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(dign_su_t1997, dignified_death__autonomy_primary, suppression_requirement, 1997, 0.48).
narrative_ontology:measurement(dign_su_t2002, dignified_death__autonomy_primary, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement(dign_su_t2009, dignified_death__autonomy_primary, suppression_requirement, 2009, 0.58).
narrative_ontology:measurement(dign_su_t2015, dignified_death__autonomy_primary, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(dign_su_t2021, dignified_death__autonomy_primary, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(dign_su_t2025, dignified_death__autonomy_primary, suppression_requirement, 2025, 0.57).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'dignified death' covers three structurally distinct constraints that share a kernel but differ in victim set, beneficiary structure, and epsilon. This story (autonomy_primary) authors epsilon for the standing prohibition-and-gatekeeping arrangement as seen from the self-determination seat. sanctity_primary evaluates the same landscape from the intrinsic-value seat, where permissive access rather than denial is the violation side. relational_autonomy redistributes decision authority across the triad, changing who counts as agenda-setter. The upstream/downstream pressure runs in both directions: autonomy litigation supplies the vocabulary permissive regimes adopt, while sanctity and relational objections supply the safeguards that thicken the gate. Each file links the other two through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
