% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__individual_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_boundary__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading — Operative Clause Doctrine
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   The Second Amendment's text reads: 'A well regulated Militia, being
 *   necessary to the security of a free State, the right of the people to
 *   keep and bear Arms, shall not be infringed.' This constraint story
 *   instantiates ONE reading of this contested kernel: the individual-right
 *   reading, under which the operative clause ('the right of the people to
 *   keep and bear Arms, shall not be infringed') establishes a pre-existing
 *   individual right to own firearms for lawful purposes (self-defense,
 *   sport, deterrence against tyranny), and the prefatory clause ('A well
 *   regulated Militia, being necessary to the security of a free State')
 *   states a purpose or context but does not limit the right's scope. This
 *   reading was elevated to constitutional doctrine by DC v. Heller (2008)
 *   and McDonald v. Chicago (2010). It generates a distinctive
 *   beneficiary/victim structure: manufacturers and owners benefit from
 *   market protection and constitutional status; those harmed by unrestricted
 *   firearm access (mass shooting victims, domestic violence survivors,
 *   suicide completers) bear extraction. This reading COEXISTS with sibling
 *   readings (militia-conditioned, insurrectionist) rather than foreclosing
 *   them — all three remain live intellectual and political positions. The
 *   constraint is classified as TANGLED ROPE because it coordinates (resolves
 *   interpretive ambiguity about the amendment's meaning) AND extracts
 *   asymmetrically (protects one reading's preferred policy outcome while
 *   harming those who would prefer alternative regulations).
 *
 * KEY AGENTS:
 *   - supreme_court_originalist_coalition: Sets the constitutional boundary through judicial interpretation and appellate review; administers the constraint through heightened scrutiny of firearm regulation; institutional power; analytical exit.
 *   - gun_manufacturers_and_distributors: Beneficiary; powerful institutional actor; benefits from market shielding and reduced regulatory vulnerability; arbitrage-capable exit.
 *   - private_firearm_owners: Beneficiary; organized collective; experience constitutional protection and reduced regulatory barriers; mobile exit.
 *   - mass_shooting_victims: Payer; powerless; harmed through death and injury; trapped (deceased parties cannot exit).
 *   - domestic_violence_survivors: Payer; moderate organized power; harmed through elevated lethality risk; constrained exit (fleeing requires safety the constraint threatens).
 *   - communities_bearing_firearm_injury_burden: Payer; moderate power (public health institutions); bear diffuse injury burden; constrained by constitutional barriers to preventive regulation.
 *   - gun_regulation_advocates: Payer/observer dual role; organized; face constitutional barriers to proposed regulations; mobile but strategically constrained by doctrine.
 *   - militia_reading_proponents: Excluded; intellectual and political representation persists despite Supreme Court rejection; trapped in minority doctrine.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__individual_right_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__individual_right_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_boundary__individual_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__individual_right_reading, "Second Amendment Individual Right Reading — Operative Clause Doctrine").
narrative_ontology:topic_domain(second_amendment_boundary__individual_right_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__individual_right_reading, '8fe68795-9545-414f-884f-316fe2e6b09c').
narrative_ontology:cs_kernel_codification('8fe68795-9545-414f-884f-316fe2e6b09c', fixed_text).
narrative_ontology:cs_authority_grounding('8fe68795-9545-414f-884f-316fe2e6b09c', lineage).
narrative_ontology:cs_interpretation_layer_present('8fe68795-9545-414f-884f-316fe2e6b09c').
narrative_ontology:cs_reading_relation('8fe68795-9545-414f-884f-316fe2e6b09c', second_amendment_boundary__militia_conditioned_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fe68795-9545-414f-884f-316fe2e6b09c', second_amendment_boundary__insurrectionist_reading, coexists_with).
narrative_ontology:cs_axiom('8fe68795-9545-414f-884f-316fe2e6b09c', foundational, operative_clause_establishes_individual_right).
narrative_ontology:cs_axiom_status(operative_clause_establishes_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('8fe68795-9545-414f-884f-316fe2e6b09c', operative_clause_establishes_individual_right, empirically_contingent).
narrative_ontology:cs_axiom('8fe68795-9545-414f-884f-316fe2e6b09c', foundational, prefatory_clause_expresses_purpose_not_limit).
narrative_ontology:cs_axiom_status(prefatory_clause_expresses_purpose_not_limit, holdable).
narrative_ontology:cs_axiom_grounding('8fe68795-9545-414f-884f-316fe2e6b09c', prefatory_clause_expresses_purpose_not_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('8fe68795-9545-414f-884f-316fe2e6b09c', operative_clause_individual_right_primacy).
narrative_ontology:cs_drift_state('8fe68795-9545-414f-884f-316fe2e6b09c', contemporary_public_health_challenge, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8fe68795-9545-414f-884f-316fe2e6b09c', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_boundary__individual_right_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, gun_manufacturers_and_distributors).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, private_firearm_owners).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, constitutional_originalists).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, mass_shooting_victims).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, domestic_violence_survivors).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, suicide_completers_by_firearm).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, communities_bearing_firearm_injury_burden).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__individual_right_reading, originalist_legal_scholars).
narrative_ontology:constraint_victim(second_amendment_boundary__individual_right_reading, gun_regulation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Second Amendment text to establish an individual right to bear arms for lawful purposes, independent of militia participation. Sets constitutional doctrine through judicial decisions (principally DC v. Heller, McDonald v. Chicago, NYSRPA v. Bruen). Treats state regulation as presumptively infringing a pre-existing right. Administers the constraint through appellate review of firearms statutes, blocking many regulatory measures.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, supreme_court_originalist_coalition, agenda_setter,
    institutional, generational, analytical, national).

% Access a market shielded from comprehensive regulation by constitutional doctrine that treats firearms as protected consumer goods. Sell across state lines within the constraint of this reading; face reduced liability risk from certain regulatory approaches (assault weapon bans, magazine capacity limits struck down under this doctrine). Benefit from the originalist interpretation that expands the domain of permissible possession.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_manufacturers_and_distributors, beneficiary,
    powerful, generational, arbitrage, national).

% Possess firearms with constitutional protection claimed to extend beyond historical militia contexts to self-defense and lawful sport. Experience reduced regulatory barriers to acquisition, retention, and carry. Organized through advocacy groups and political mobilization that defend this reading's permanence. Can relocate to more permissive jurisdictions if state-level regulation tightens.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, private_firearm_owners, beneficiary,
    organized, biographical, mobile, national).

% Killed or wounded in mass-shooting events. Bear the constraint through injury, death, and trauma. Their absence from the policymaking table (they are deceased or recover outside the political process) is structural — they cannot advocate for their own survival. The constraint's durability partly depends on this structural invisibility.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, mass_shooting_victims, payer,
    powerless, immediate, trapped, national).

% Face elevated lethality risk from intimate partners with firearm access. Are harmed by the constraint's operation — the individual-right reading impedes comprehensive domestic violence firearm restrictions (red-flag laws, temporary removal orders) by raising constitutional barriers. Can exit relationships, but exit itself carries risk when firearms are present.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, domestic_violence_survivors, payer,
    moderate, biographical, constrained, national).

% Individuals who died by firearm suicide. Bear the constraint through completed suicide; the constraint's operation (permissive access reducing friction to rapid, high-lethality means) directly contributes to completed suicide risk. Represented through proxy epidemiological data; their absence from political voice is absolute.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, suicide_completers_by_firearm, payer,
    powerless, immediate, trapped, national).

% Bear collective harm from firearm injury prevalence: emergency department capacity strain, trauma-surgeon burnout, public health infrastructure dedicated to firearm injury (ballistic wound management, trauma registries, rehabilitation). Experience this burden diffusely; organizing to change the constraint faces constitutional barriers now codified.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, communities_bearing_firearm_injury_burden, payer,
    moderate, biographical, constrained, local).

% Seek comprehensive firearm regulation (universal background checks, permit requirements, licensing, safe-storage mandates, assault-weapon restrictions). Face constitutional barriers created by this reading's doctrine: regulations they propose are vulnerable to strike-down under heightened constitutional scrutiny. Operate within a constraint that has redefined the policy space.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, gun_regulation_advocates, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__individual_right_reading, gun_regulation_advocates, observer).

% Hold an alternative constitutional reading (militia-conditioned, collective-defense framing). Are locked out of Supreme Court doctrine as operative law, despite their textual and historical arguments (prefatory clause as limiting). Their absence from the Court's majority coalition means their reading is excluded from constitutional doctrine, even though it remains intellectually and politically contested.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, militia_and_collective_defense_reading_proponents, excluded,
    powerful, biographical, trapped, national).

% Study firearm injury epidemiology, access patterns, and regulatory effectiveness. Operate in a constraint that has politically and legally narrowed the policy options they can study (certain regulations become unconstitutional under this reading). Their empirical findings about harm are framed as constraining evidence within a constitutional boundary they did not set.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, public_health_researchers, observer,
    organized, biographical, mobile, national).

% Build jurisprudential careers on originalist constitutional interpretation. This reading's success vindicates their methodological commitment (reading the Constitution according to its original public meaning rather than evolving standards). Their institutional authority and influence are heightened by the constraint's operative status.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__individual_right_reading, originalist_legal_scholars, beneficiary,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_boundary__individual_right_reading, gun_manufacturers_and_distributors).
narrative_ontology:fixing_cost_class(second_amendment_boundary__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, judicially administrable boundary between protected and regulable firearm ownership by fixing the Second Amendment's meaning: the operative clause grants an individual right; the prefatory militia clause expresses a purpose but does not limit scope. This resolves interpretive ambiguity and provides predictability for litigation.
% TRANSFER_FUNCTION: Transfers constitutional legitimacy from regulation-permissive readings to an ownership-protective reading. Moves regulatory authority from legislatures (which can enact comprehensive gun laws) to courts (which apply heightened constitutional scrutiny). Channels injury burden (mass shootings, domestic violence, suicide) to those harmed by unrestricted access while channeling market protection and constitutional status to manufacturers and owners.
% ABSENT_VOICES: Mass shooting victims are deceased. Domestic violence survivors and suicide-attempt survivors are underrepresented in constitutional policymaking. Communities bearing the injury burden (emergency departments, trauma services, public health) are not seated at constitutional interpretation. Militia-reading proponents are excluded from Supreme Court doctrine despite remaining intellectually and legislatively represented. These absent and excluded seats would argue for prefatory-clause limiting power and comprehensive regulation.
% DISAPPEARANCE_RATIONALE: If this reading's doctrine disappeared, legislatures would reinstate comprehensive firearm regulation (universal background checks, permit systems, assault-weapon restrictions, magazine limits, red-flag laws, safe-storage requirements) within months to years. The firearms market would contract under regulatory pressure. Constitutional protection for private ownership would evaporate, replaced by the militia-reading's collective-defense framing or prohibition-permissive doctrine. Manufacturers' market position and owners' constitutional status would fundamentally alter.
% FOUNDING_PROBLEM: The Second Amendment's text contains an apparent tension between the prefatory militia clause and the operative clause protecting bear arms. Early jurisprudence (19th-20th centuries) read the prefatory clause as limiting; later originalist scholarship (1970s onward) argued the operative clause establishes a pre-existing, militia-independent right. The founding problem was interpretive: which reading captures the original public meaning?
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars (Sanford Levinson, Eugene Volokh, Brannon Denning) and the DC v. Heller majority opinion attest the interpretive problem was genuine and the individual-right reading is the correct historical account. Non-originalist scholars (Cass Sunstein, Erwin Chemerinsky, Saul Cornell) and dissenting opinions (Justice Breyer, Justice Sotomayor) attest the militia reading is textually and historically defensible and the individual-right reading misreads the founding context. The disagreement persists among legal historians and constitutional scholars; no independent authority external to the legal profession has adjudicated the interpretive contest.
narrative_ontology:disappearance_verdict(second_amendment_boundary__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__individual_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__individual_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68 at interval end) reflects the asymmetric distribution of gains and harms: manufacturers gain market protection and reduced liability risk; owners gain constitutional status and reduced regulatory burden; those harmed by unrestricted access bear injury and death without compensation or political voice. Suppression (0.72) reflects the constraint's operation through judicial power: regulations proposed to reduce firearm injury (background checks, permit systems, red-flag laws, safe-storage mandates) are blocked or vulnerable to constitutional challenge under this reading. Theater ratio (0.44) reflects that public and political discourse frames the constraint as fidelity to original constitutional meaning (a neutral interpretive project) while the actual operation redistributes risk and protects industry. Accessibility collapse (0.48) is moderate-low because alternative readings and regulatory proposals remain intellectually and legislatively active — the constraint does not collapse alternatives, it elevates one reading to constitutional authority while leaving others contestable. Resistance (0.71) is high because the constraint meets sustained opposition from public-health advocates, gun-regulation movements, and competing constitutional readings. The measurement trajectory shows extractiveness and suppression rising from t0 to t3 (post-Heller, doctrine crystallizes and regulatory challenge increases) and plateauing from t12 onward (doctrine stabilizes; secondary expansion via New York State Rifle Association v. Bruen extends it but does not substantially increase extraction or suppression magnitudes). Theater ratio rises throughout (increasingly, enforcement focuses on defending the reading's legitimacy rather than administering straightforward coordination) but remains below 0.5 (the constraint's coordination function is not purely theatrical — interpretive consensus remains valued).
 *
 * PERSPECTIVAL GAP:
 *   The originalist-coalition seat and the gun-owner beneficiary seats experience this as genuine coordination: resolving textual ambiguity, establishing a stable legal boundary, protecting a pre-existing right. The payer seats (families of mass shooting victims, domestic violence survivors, public health institutions) experience the same structural mechanism as enforced extraction: the constraint blocks regulations they believe would reduce preventable harm, justified by a constitutional reading they contest. The regulation-advocate seats experience a paradoxical position: they operate within the constraint's boundaries (using legislative advocacy, state-level subterfuge, litigation seeking reinterpretation) while resisting the constraint itself. The militia-reading-proponent seat is locked out: intellectually and legislatively represented but Supreme Court doctrine forecloses their reading from constitutional authority. Per-seat classifications should diverge: originalists compute mountain-to-rope (natural law anchored in text); manufacturers compute rope (coordination with stable market benefit); victims compute snare (pure extraction under color of constitutional authority); regulation advocates compute snare (extraction blocking preferred policy).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (manufacturers, owners, originalists) have low directionality (d near 0.0–0.3): the constraint protects them, provides market access and constitutional status, and requires no exit. Payers (victims, survivors, public health, regulation advocates) have high directionality (d near 0.7–1.0): the constraint extracts from them (blocks protective regulation, protects the reading that allows harm to persist), and exit is trapped or identity-locked (mass shooting survivors are deceased; domestic violence survivors are bound to the risk through relational status; public health is institutionally bound to caring for injury consequences). The originalist coalition sits near the beneficiary end (d ~0.2) despite being the agenda-setter because the constraint protects their interpretive authority; exit is analytical (they could lose cases and reinterpret, but institutional incentives favor the current reading). Directionality overrides are unnecessary — the derived values from beneficiary/victim + exit options capture the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents false lumping of coordination and extraction. A naive reading might classify the entire mechanism as pure coordination (interpreting the Constitution is coordination, establishing legal certainty is coordination) or pure extraction (protecting industry interests is extraction, blocking regulation is extraction). The tangled-rope classification captures both: the constraint genuinely solves an interpretive coordination problem (which reading is the law?) AND asymmetrically extracts (protects one policy outcome, harms alternative regulatory visions and those who would benefit from them). Mandatrophy appears in the founding-problem analysis: the founding problem (interpretive ambiguity) is live in the intellectual community but legally dead in Supreme Court doctrine — the constraint perpetuates itself not because the founding problem remains unresolved but because institutional actors benefit from the current resolution. The measured theater ratio (rising to 0.44) suggests increasing reliance on legitimacy narrative (fidelity to original meaning) to defend the reading against empirical harm data (mass shooting epidemiology, suicide lethality risk).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_determinacy,
    'What was the original public meaning of the Second Amendment at ratification (1791), and does it unambiguously support the individual-right reading or the militia-conditioned reading?',
    'Historical analysis of 18th-century usage, founding-era state constitutions and court decisions, contemporaneous political discourse, and the intentions of the framers and ratifiers. Expert historical consensus (or persistent disagreement) from non-partisan sources.',
    'If historical analysis confirms the individual-right reading, the constraint is grounded in genuine constitutional meaning and the originalist interpretation is vindicated. If historical analysis favors the militia reading or shows genuine ambiguity, the individual-right reading is shown to be a selective reading imposed by modern jurisprudence, moving the constraint from mountain-like (textually grounded) to snare-like (invented tradition masking extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_determinacy, empirical, 'Whether the individual-right reading accurately recovers the Second Amendment''s original public meaning or imposes modern policy preferences onto the text.').

omega_variable(
    causality_of_firearm_access_to_harm,
    'To what extent does permissive firearm access directly cause elevated rates of mass shooting, domestic violence lethality, and suicide completion (as opposed to other social, economic, or psychological factors)?',
    'Epidemiological studies comparing harm rates across jurisdictions with different access regimes, controlling for confounders. Meta-analysis of causal inference evidence. International comparison of similar populations with different regulatory regimes.',
    'If causality is strong and specific to access/permissiveness, the harm born by victims is directly traceable to the constraint''s operation, solidifying the snare classification. If causality is weak or confounded by other factors, the constraint''s role is more attenuated, potentially weakening the victim claim and moving the classification toward rope or even mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_of_firearm_access_to_harm, empirical, 'Whether the measured harm to victims is causally driven by the constraint''s operation or is attributable to other factors.').

omega_variable(
    constitutional_reading_incommensurability,
    'Are the individual-right reading and the militia-conditioned reading genuinely incommensurable (logically incompatible), or can they be reconciled as emphasizing different aspects of a coherent constitutional principle?',
    'Constitutional legal theory: analysis of whether the readings rest on incompatible premises about the prefatory clause''s function, the scope of ''the right of the people,'' and the relationship between constitutional text and constitutional purpose.',
    'If the readings are incommensurable, the constraint is a choice between competing visions, not the discovery of a single truth — the originalist framing of the constraint as neutral interpretation is undermined. If reconcilable, the constraint may be reframed as one defensible emphasis within a broader constitutional settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_reading_incommensurability, conceptual, 'Whether the individual-right and militia-conditioned readings are logically incompatible or represent different emphases within a coherent constitution.').

omega_variable(
    identity_lock_of_originalist_authority,
    'To what extent is originalist constitutional interpretation (as a method) institutionally locked into defending the individual-right reading because the reading''s success vindicates originalism as a methodology?',
    'Institutional analysis of originalist jurisprudence: cases where originalist method would support non-individual-right conclusions; analysis of whether originalist scholars and judges defend the individual-right reading on methodological grounds or on independent policy grounds disguised as methodology.',
    'If originalist authority is identity-locked to defending the individual-right reading, the constraint''s persistence is reinforced by methodological institutional identity, not neutral textual analysis — the constraint becomes more snare-like (extraction protected by institutional authority that benefits from defending it). If originalism is methodologically capable of reaching other conclusions, the reading is less locked in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_of_originalist_authority, conceptual, 'Whether originalist constitutional methodology is independent of the individual-right reading or is institutionally bound to defending it.').

omega_variable(
    suppression_mechanism_legal_vs_epistemic,
    'Is the measured suppression (0.72) primarily structural (legal barriers to regulation, heightened scrutiny making some laws unconstitutional) or internalized/epistemic (gun-regulation advocates internalize the reading as legitimate constitutional law even when they disagree with its policy consequences)?',
    'Behavioral analysis: do regulation advocates cease proposing regulations because legal barriers make passage impossible, or because they have internalized the constitutional boundary as legitimate? Counterfactual: if the Supreme Court reversed the reading tomorrow, would the same advocates suddenly propose regulations they had abandoned, or would they require epistemic deprogramming?',
    'If suppression is primarily structural, exit from the constraint requires only legal reversal or jurisdiction-shopping. If suppression is internalized, the constraint persists even after legal barriers are removed — targets carry the suppression with them and may not reconstruct regulatory options even when legally permitted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legal_vs_epistemic, empirical, 'Whether suppression of gun-regulation advocacy is structural (legal barriers) or internalized (epistemic acceptance of the constitutional boundary).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__individual_right_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__individual_right_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(seco_tr_t0, projected).
narrative_ontology:measurement(seco_tr_t3, second_amendment_boundary__individual_right_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(seco_tr_t3, observed).
narrative_ontology:measurement(seco_tr_t6, second_amendment_boundary__individual_right_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(seco_tr_t6, observed).
narrative_ontology:measurement(seco_tr_t9, second_amendment_boundary__individual_right_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement_basis(seco_tr_t9, observed).
narrative_ontology:measurement(seco_tr_t12, second_amendment_boundary__individual_right_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(seco_tr_t12, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_boundary__individual_right_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_boundary__individual_right_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(seco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__individual_right_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(seco_be_t0, projected).
narrative_ontology:measurement(seco_be_t3, second_amendment_boundary__individual_right_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement_basis(seco_be_t3, observed).
narrative_ontology:measurement(seco_be_t6, second_amendment_boundary__individual_right_reading, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(seco_be_t6, observed).
narrative_ontology:measurement(seco_be_t9, second_amendment_boundary__individual_right_reading, base_extractiveness, 9, 0.63).
narrative_ontology:measurement_basis(seco_be_t9, observed).
narrative_ontology:measurement(seco_be_t12, second_amendment_boundary__individual_right_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(seco_be_t12, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_boundary__individual_right_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_boundary__individual_right_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(seco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__individual_right_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(seco_su_t0, projected).
narrative_ontology:measurement(seco_su_t3, second_amendment_boundary__individual_right_reading, suppression_requirement, 3, 0.61).
narrative_ontology:measurement_basis(seco_su_t3, observed).
narrative_ontology:measurement(seco_su_t6, second_amendment_boundary__individual_right_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement_basis(seco_su_t6, observed).
narrative_ontology:measurement(seco_su_t9, second_amendment_boundary__individual_right_reading, suppression_requirement, 9, 0.69).
narrative_ontology:measurement_basis(seco_su_t9, observed).
narrative_ontology:measurement(seco_su_t12, second_amendment_boundary__individual_right_reading, suppression_requirement, 12, 0.71).
narrative_ontology:measurement_basis(seco_su_t12, observed).
narrative_ontology:measurement(seco_su_t15, second_amendment_boundary__individual_right_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(seco_su_t15, observed).
narrative_ontology:measurement(seco_su_t20, second_amendment_boundary__individual_right_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(seco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__individual_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__militia_conditioned_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, firearm_injury_epidemiology_constraint).
narrative_ontology:affects_constraint(second_amendment_boundary__individual_right_reading, state_level_firearms_regulation_constraint).

% DUAL FORMULATION NOTE:
% The second_amendment_boundary kernel decomposes into three structurally distinct constraints corresponding to three live readings of the same text. Each reading instantiates different beneficiary/victim structures, different extractiveness profiles, and different regulatory consequences. The individual_right_reading (this constraint) coexists with the militia_conditioned_reading and insurrectionist_reading; all three readings remain live intellectual and political positions despite Supreme Court elevation of the individual_right_reading to operative doctrine. This constraint affects downstream constraints that depend on how the Second Amendment is read (state regulation feasibility, epidemiological research capacity, etc.).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
