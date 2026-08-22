% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as State Militia-Authority Guarantee (Collective Right Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the collective-right reading of the Second
 *   Amendment kernel: the constitutional text protects state institutional
 *   authority to organize and arm militias, not a freestanding individual
 *   right to own firearms. This reading dominated federal appellate
 *   jurisprudence for most of the twentieth century (culminating in the
 *   ambiguous United States v. Miller, 1939, and near-uniform circuit court
 *   adherence through the late 20th century) before District of Columbia v.
 *   Heller (2008) displaced it with the individual-right reading at the
 *   Supreme Court level. Under this reading, gun control legislation faces
 *   low constitutional friction because no individual right is burdened;
 *   extraction is low because the reading's operative effect is regulatory
 *   permission, not resource transfer, and its beneficiary set (state and
 *   federal governments) is institutional rather than rent-extracting.
 *
 * KEY AGENTS:
 *   - state_governments: institutional beneficiary — retains militia-organizing authority
 *   - organized_state_militias: institutional beneficiary — the entity the clause is read to protect
 *   - federal_regulatory_authority: institutional beneficiary/agenda_setter — gains broad regulatory latitude
 *   - individual_gun_owners: payer — loses constitutional shield against firearms regulation
 *   - firearms_rights_advocacy_groups: excluded — doctrinal loser under this reading, active in the broader contest
 *   - constitutional_historians: analytical observer — contested drafting-history evidence base
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.22).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as State Militia-Authority Guarantee (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '6b067cbc-931a-4658-9b0f-0ee8db2044ec').
narrative_ontology:cs_kernel_codification('6b067cbc-931a-4658-9b0f-0ee8db2044ec', fixed_text).
narrative_ontology:cs_authority_grounding('6b067cbc-931a-4658-9b0f-0ee8db2044ec', lineage).
narrative_ontology:cs_interpretation_layer_present('6b067cbc-931a-4658-9b0f-0ee8db2044ec').
narrative_ontology:cs_reading_relation('6b067cbc-931a-4658-9b0f-0ee8db2044ec', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('6b067cbc-931a-4658-9b0f-0ee8db2044ec', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('6b067cbc-931a-4658-9b0f-0ee8db2044ec', foundational, prefatory_clause_is_limiting).
narrative_ontology:cs_axiom_status(prefatory_clause_is_limiting, holdable).
narrative_ontology:cs_axiom_grounding('6b067cbc-931a-4658-9b0f-0ee8db2044ec', prefatory_clause_is_limiting, conventional).
narrative_ontology:cs_axiom('6b067cbc-931a-4658-9b0f-0ee8db2044ec', foundational, right_belongs_to_organized_militia_not_individual).
narrative_ontology:cs_axiom_status(right_belongs_to_organized_militia_not_individual, overridden).
narrative_ontology:cs_axiom_grounding('6b067cbc-931a-4658-9b0f-0ee8db2044ec', right_belongs_to_organized_militia_not_individual, empirically_contingent).
narrative_ontology:cs_reference_frame('6b067cbc-931a-4658-9b0f-0ee8db2044ec', federalist_militia_preservation_framework).
narrative_ontology:cs_drift_state('6b067cbc-931a-4658-9b0f-0ee8db2044ec', post_heller_2008, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('6b067cbc-931a-4658-9b0f-0ee8db2044ec', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_state_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_regulatory_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, individual_gun_owners).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_preserving_state_military_capacity).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, collective_security_over_individual_arms_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states retain a constitutionally protected interest in raising and maintaining their own militia forces free from federal disarmament, but the interest runs to the state as an institution, not to any individual resident. States can organize, arm, and regulate their militia bodies (historically the predecessor to the National Guard) without needing to secure an individual constitutional entitlement for private citizens.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% As institutional bodies (militia units, later National Guard formations), their collective capacity to organize and arm is what the clause is read to protect. Individual members participate through the militia structure, not through a personal right that exists independent of it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_state_militias, beneficiary,
    organized, generational, analytical, national).

% Under this reading, Congress and federal agencies retain broad latitude to regulate, restrict, or license private firearm ownership because no individual constitutional right is implicated — gun control legislation faces only rational-basis-style scrutiny rather than the heightened scrutiny an individual-right reading would impose. This reading substantially expands the space in which federal and state regulation can operate without constitutional challenge.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_regulatory_authority, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, federal_regulatory_authority, agenda_setter).

% Individuals seeking to own or carry firearms outside an organized militia context lose the constitutional shield an individual-right reading would provide. Their ability to resist restrictive firearms legislation via Second Amendment litigation is foreclosed under this reading; they must rely on statutory or state constitutional protections instead.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Organizations advocating an individual right to keep and bear arms are structurally positioned as the losing party under this reading — their preferred doctrinal framework is not the one in force. They continue litigating and lobbying for the individual-right reading but, within a jurisdiction adopting the collective reading, their constitutional argument carries no force.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, firearms_rights_advocacy_groups, excluded,
    organized, generational, mobile, national).

% Study the militia clause's drafting history, founding-era militia statutes, and post-ratification practice to assess whether the operative clause ('a well regulated Militia') controls the scope of the right or is merely prefatory. Their scholarship is invoked by all three readings as supporting evidence, contested at every turn.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, diffuse).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reserves to the states, against federal disarmament, the institutional capacity to raise and equip organized militia forces — a federalism-preserving allocation of military authority between federal and state governments that was live during the founding era's distrust of standing federal armies.
% TRANSFER_FUNCTION: Moves constitutional protection away from private individual firearms ownership and toward state institutional military capacity; correspondingly, it moves regulatory latitude toward federal and state legislatures, who gain the ability to restrict private gun ownership without triggering heightened constitutional scrutiny.
% ABSENT_VOICES: Individual gun owners and firearms rights organizations would object that this reading strips a right they believe was always personal and freestanding; they are not absent from the broader controversy but are the losing party wherever courts adopt this specific reading, so their objection carries no doctrinal weight within it.
% DISAPPEARANCE_RATIONALE: If this reading vanished as controlling doctrine (as it effectively did after District of Columbia v. Heller, 2008, adopted the individual-right reading), federal and state firearms regulation would face substantially heightened constitutional scrutiny, and gun control legislation predicated on the absence of an individual right would become vulnerable to challenge — a real-world rearrangement already observed. Whether the pre-Heller collective-right jurisprudence (e.g., United States v. Miller's ambiguous holding) actually 'protected' states as opposed to merely failing to recognize an individual right is itself contested among historians.
% FOUNDING_PROBLEM: The founding generation distrusted permanent federal standing armies and wanted assurance that state militias — composed of the armed citizenry organized under state authority — could not be disarmed by federal action, preserving a decentralized check on federal military power.
% FOUNDING_PROBLEM_CORROBORATION: Military historians and federalism scholars outside the modern gun-control debate broadly agree that the organized state militia of the founding era no longer exists in any functionally equivalent form — it was superseded by the National Guard (a federally integrated force under the Militia Act of 1903 and subsequent statutes) and by a professional standing federal military that the founders' arrangement was specifically designed to check. Neither gun-control advocates nor gun-rights advocates dispute that the literal founding-era militia is defunct; they dispute what follows doctrinally from that fact, which is exactly the site of the kernel contest.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading's structural effect is permissive (it clears space for legislatures to regulate) rather than extractive (it does not transfer resources from a victim class to a beneficiary class in the way a snare or tangled rope would). Suppression is moderate-low (0.22): the reading does not suppress alternative doctrinal readings by force, but its adoption by controlling courts does foreclose individual-right litigation strategies within that jurisdiction until overruled. Resistance is authored high (0.75) reflecting the sustained, well-organized, and ultimately successful campaign by individual-rights advocates and originalist legal scholars to displace this reading at the Supreme Court level in Heller — this reading was never uncontested and was actively resisted for decades before its 2008 defeat. Accessibility collapse is moderate (0.35): alternative readings remained visible and litigated throughout the twentieth century; the collective reading never achieved the kind of near-total closure a genuine mountain would show.
 *
 * DIRECTIONALITY LOGIC:
 *   State and federal governmental bodies are the structural beneficiaries because the reading expands their regulatory and organizational authority (low d, near the beneficiary end). Individual gun owners bear the cost of foreclosed constitutional argument (higher d, payer role) but are not 'victims' in the extraction sense — no resource or rent moves to a beneficiary at their expense; they simply lose a doctrinal tool. This is why victims[] is left empty even though a payer stakeholder is named: the payer bears a lost-litigation-option cost, not an extractive transfer, which keeps the claimed_type at rope rather than tangled_rope or snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (checking federal standing-army power via state militia capacity) is dead: the organized state militia of 1791 was absorbed into the federally-integrated National Guard by the early twentieth century, and no state today maintains an independent militia force capable of checking federal military power in the founders' sense. This reading's doctrinal life during 1939-2008 persisted not because the founding function remained live, but because it served the separate, functioning purpose of preserving broad legislative latitude over firearms regulation — a genealogical substitution the R5 corroboration surfaces: historians outside the gun debate agree the literal founding militia is gone, while gun-control and gun-rights advocates dispute only what follows from that fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operative_force,
    'Does the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') limit the scope of the operative clause (''the right of the people to keep and bear Arms, shall not be infringed''), or is it merely explanatory and non-limiting?',
    'Historical linguistic analysis of eighteenth-century constitutional drafting conventions, comparison to contemporaneous state constitutional militia clauses, and founding-era legal commentary on prefatory-clause construction; this is the central textual dispute the three sibling readings resolve differently.',
    'If the prefatory clause is held limiting, the collective-right reading (or civic-right reading) is textually supported; if held non-limiting, the individual-right reading prevails, as the Supreme Court held in Heller. This single interpretive choice is the primary fork between the three sibling constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_operative_force, conceptual, 'Whether the militia preamble limits or merely explains the arms-bearing right.').

omega_variable(
    founding_militia_functional_successor,
    'Is the modern National Guard the functional successor to the founding-era militia such that the state-authority interest this reading protects is still institutionally embodied, or has that interest gone extinct with no living successor?',
    'Institutional history of the Militia Act of 1903 and subsequent federalization statutes; comparison of Guard command structure (dual state/federal control) to founding-era militia command (purely state control).',
    'If the National Guard is a genuine successor, the collective reading''s beneficiary (state militia authority) still has a living referent; if the federalization was so complete that state militia autonomy no longer meaningfully exists, this reading protects an institution that has already been absorbed, weakening its claim to solve a live founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_militia_functional_successor, empirical, 'Whether the National Guard preserves the state militia autonomy the founding clause protected.').

omega_variable(
    reading_selection_framing,
    'Is the more defensible framing of this kernel the text-and-drafting-history framing (which favors decomposing into these three readings) versus a doctrinal-precedent framing (which would center United States v. Miller''s actual — narrower and more ambiguous — holding rather than the broader ''collective right'' gloss later commentators attached to it)?',
    'Close reading of Miller''s actual holding (that the Court lacked judicial notice a sawed-off shotgun had militia utility) versus the broader collective-right doctrine subsequently attributed to it by circuit courts.',
    'Under the doctrinal-precedent framing, this reading''s ε might be authored somewhat higher, reflecting that circuit courts extended Miller well beyond its narrow holding to build the collective-right edifice — an extension some scholars read as itself a form of doctrinal overreach. Under the text-and-drafting framing used here, ε stays low because the reading is treated as a faithful (if ultimately unsuccessful) textual construction rather than an overreach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_framing, conceptual, 'Whether to frame this reading via original text/drafting history or via how far circuit courts extended Miller''s narrow holding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1871, second_amendment_scope__collective_right_reading, theater_ratio, 1871, 0.08).
narrative_ontology:measurement_basis(seco_tr_t1871, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__collective_right_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_scope__collective_right_reading, theater_ratio, 2026, 0.15).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1871, second_amendment_scope__collective_right_reading, base_extractiveness, 1871, 0.12).
narrative_ontology:measurement_basis(seco_be_t1871, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__collective_right_reading, base_extractiveness, 1980, 0.17).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.22).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_scope__collective_right_reading, base_extractiveness, 2026, 0.18).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_scope__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% Part of the second_amendment_scope kernel family (3 stories). This story (collective_right_reading) authors low ε reflecting a narrow institutional beneficiary set (states/militias) and broad regulatory permission. The sibling individual_right_reading authors a structurally different ε reflecting a personal constitutional entitlement with different victims (regulators facing heightened scrutiny) and different beneficiaries (individual owners, firearms industry). The sibling civic_right_reading occupies a middle position. All three share the same constitutional text but are ε-invariant as separate constraints per the decomposition principle — measuring 'the Second Amendment' one way versus another yields different ε, so they are authored as three linked stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
