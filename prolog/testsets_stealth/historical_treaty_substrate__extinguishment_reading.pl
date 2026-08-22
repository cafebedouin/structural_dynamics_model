% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Extinguishment Reading — Historical Treaties as Completed Conveyances
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   Historical treaties between Indigenous nations and the settler state are
 *   read here as completed property transactions: the nations ceded
 *   territorial sovereignty, and the state's return consideration — defined
 *   reserves, unindexed per-capita annuities, enumerated harvesting rights —
 *   discharged whatever was owed, vesting sole legitimate authority over the
 *   ceded territory in the settler state. This file authors THAT reading and
 *   nothing else. Per the epsilon-invariance principle, the colloquial label
 *   'what the historical treaties mean' covers structurally distinct claims
 *   with different extraction profiles, beneficiary/victim sets, and failure
 *   modes; the sibling readings (stewardship_reading,
 *   nation_to_nation_reading) are separate constraint stories linked through
 *   network.affects_constraints, and the contest itself is routed to omega
 *   variables rather than folded into this constraint's classification. The
 *   referent of every metric below is the standing extinguishment arrangement
 *   — the post-treaty order of reserve confinement, fixed consideration, and
 *   unitary state jurisdiction — assessed by this reading's own lights, never
 *   the arrangements the sibling readings would install.
 *
 * KEY AGENTS:
 *   - - settler_governments: Agenda-setting beneficiary (institutional/arbitrage) — administers ceded territory as sole authority, issues titles, pays fixed annuities
 *   - - signatory_indigenous_nations: Primary target (organized/identity_locked) — confined to reserves and enumerated rights; secondary recipient of the narrow treaty benefits
 *   - - resource_and_agricultural_settlers: Derivative beneficiary (powerful/mobile) — holds granted fee-simple title and tenures on ceded lands
 *   - - metis_and_nonstatus_descendants: Excluded party (powerless/trapped) — inside the produced jurisdictional order, outside the settlement
 *   - - hereditary_leadership_structures: Excluded party (powerless/identity_locked) — consent structures bypassed at negotiation; hold the oral undertakings
 *   - - superior_courts: Authoritative interpreter (institutional/analytical) — applies and modulates the reading through doctrine
 *   - - international_rights_bodies: Analytical observer (moderate/analytical) — reviews the arrangement against free, prior and informed consent standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.35).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.72).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Extinguishment Reading — Historical Treaties as Completed Conveyances").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '412de46f-327a-493d-b288-10b63a550194').
narrative_ontology:cs_kernel_codification('412de46f-327a-493d-b288-10b63a550194', formalized).
narrative_ontology:cs_authority_grounding('412de46f-327a-493d-b288-10b63a550194', lineage).
narrative_ontology:cs_interpretation_layer_present('412de46f-327a-493d-b288-10b63a550194').
narrative_ontology:cs_reading_relation('412de46f-327a-493d-b288-10b63a550194', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_reading_relation('412de46f-327a-493d-b288-10b63a550194', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_axiom('412de46f-327a-493d-b288-10b63a550194', foundational, cession_conclusive_upon_ratification).
narrative_ontology:cs_axiom_status(cession_conclusive_upon_ratification, holdable).
narrative_ontology:cs_axiom_grounding('412de46f-327a-493d-b288-10b63a550194', cession_conclusive_upon_ratification, conventional).
narrative_ontology:cs_axiom('412de46f-327a-493d-b288-10b63a550194', secondary, fixed_consideration_bars_reopening).
narrative_ontology:cs_axiom_status(fixed_consideration_bars_reopening, holdable).
narrative_ontology:cs_axiom_grounding('412de46f-327a-493d-b288-10b63a550194', fixed_consideration_bars_reopening, conventional).
narrative_ontology:cs_reference_frame('412de46f-327a-493d-b288-10b63a550194', completed_conveyance_order).
narrative_ontology:cs_drift_state('412de46f-327a-493d-b288-10b63a550194', contemporary_undrip_implementation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('412de46f-327a-493d-b288-10b63a550194', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, resource_and_agricultural_settlers).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, signatory_indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, signatory_indigenous_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, doctrine_of_extinguishment).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, crown_radical_title_presumption).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, completed_instrument_finality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and provincial or state governments hold and administer the ceded territories as the sole legitimate authority under this reading. They issued the patents, statutes, and grants that allocate the land; they operate the departments that administer reserve lands and pay the fixed per-capita annuities; they accept, reject, or settle specific claims by policy. Their exit is not departure but reframing: they can amend the governing statutes, redefine the settlement's administration, or negotiate new instruments, and they face no competing authority over the territory.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Nations that signed the written instruments receive reserve parcels, per-capita annuities fixed in nominal dollars and never indexed, and enumerated hunting, fishing, and gathering rights exercisable subject to regulation. Beyond the reserve boundary they hold no governmental jurisdiction under this reading; their collective life is organized around administering the narrow entitlements the transaction defined. Leaving is not available in any ordinary sense: the nation's membership, territory, and legal personality are constituted through the treaty relationship, so pursuing the entitlements and dissolving the relationship are the same act. They litigate specific claims and rights cases inside the framework the reading supplies.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, signatory_indigenous_nations, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, signatory_indigenous_nations, beneficiary).

% Farmers, ranchers, timber, mining, and energy operators hold fee-simple titles, leases, and tenures granted out of the ceded territory. What flows to them is the operative value of the transaction: secure, mortgageable, transferable property under a single legal system. Their connection to the treaty terms themselves is indirect; they can divest and relocate, and most do not participate in the arrangement's administration.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, resource_and_agricultural_settlers, beneficiary,
    powerful, biographical, mobile, regional).

% Communities descended from peoples present in the treaty territories but left outside the written terms — Metis bands at the negotiation grounds, descendants stripped of status by enfranchisement rules — live inside the jurisdictional order the transaction produced while inheriting neither reserve tenure nor annuity entitlement. They have no seat in the settlement and no instrument to enforce; their objection is that the transaction was executed over them without their inclusion.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, metis_and_nonstatus_descendants, excluded,
    powerless, generational, trapped, regional).

% Traditional governance bodies whose consent protocols differ from the elected-chief model the negotiation format recognized. The written instruments purport to bind peoples whose own decision-making structures were bypassed at the table; they hold the oral undertakings that accompany the texts and contest the transaction's adequacy from outside its four corners. Their standing to object is not recognized within the reading's framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, hereditary_leadership_structures, excluded,
    powerless, generational, identity_locked, regional).

% Appellate and supreme courts decide what the written instruments convey, whether cession occurred, and what obligations survive. Doctrines such as the honour of the Crown and strict construction of ambiguous terms modulate application without disturbing the frame's core. They sit above the dispute, supply the interpretive apparatus both sides litigate within, and cannot be exited by any party except through constitutional amendment.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, superior_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, superior_courts, observer).

% United Nations treaty bodies, special rapporteurs, and human-rights mechanisms review the arrangement against the free, prior and informed consent standard. They issue concluding observations, accept petitions, and file interventions; they hold no power over domestic title registries or provincial jurisdiction, and their findings bind nothing directly.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_rights_bodies, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts open-ended inter-societal relations over vast territories into a closed allocation: it fixes where each party may live and govern, substitutes defined reserve parcels, unindexed per-capita annuities, and enumerated harvesting rights for shared jurisdiction, and clears title so land can be patented, mortgaged, taxed, and transacted under a single legal system.
% TRANSFER_FUNCTION: Moves territorial jurisdiction and the alienable resource base of entire regions from signatory Indigenous nations to the settler state, which re-grants them as fee-simple titles and leases; moves back reserve tenure, nominal annuities, and defined use-rights to the nations.
% ABSENT_VOICES: Hereditary governance structures whose consent protocols the negotiation format bypassed; Metis and non-status descendants left outside the written terms; women's councils excluded from the negotiating table; and every subsequent generation bound by a transaction characterized as complete before they existed. They stand outside the constitutional conversation the reading defines — the reading's finality is precisely what keeps them out.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the land-titles system built on ceded territory would lose its root of title; provincial and state jurisdiction over the affected regions would rest on an unsettled basis; reserve boundaries, municipal incorporations, and resource tenures would all require renegotiation. Nothing about the current settlement is self-holding — it is administered continuously.
% FOUNDING_PROBLEM: Securing uncontested settler occupation: converting a frontier of overlapping sovereignty and periodic diplomacy into a settled conveyance that cleared Indigenous title so agriculture, railways, and mass immigration could proceed under one legal order.
% FOUNDING_PROBLEM_CORROBORATION: The settler state's own negotiating correspondence (the published treaty-commission records) attests the founding problem exactly as stated — securing land for immigration — so the problem's existence is corroborated outside dispute. Its status is contested: the state attests a live residual problem (title certainty requires the arrangement's continuous maintenance), while the Royal Commission on Aboriginal Peoples, scholarly treaty historiography, and oral histories admitted in specific-claims proceedings attest that the problem was the settler society's alone and that the transaction never legitimately resolved it. No source outside the beneficiary set attests the problem as resolved on this reading's terms.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).
:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is tangled_rope because the arrangement possesses both a genuine coordination function and asymmetric extraction held by active enforcement: it really does convert open-ended inter-societal relations into a closed, administrable allocation, and it really does move continental jurisdiction for consideration fixed in nineteenth-century dollars. The metrics are authored independently, descriptively, from this reading's own lights. Extractiveness sits at 0.35 — low-moderate rather than negligible — because a concluded purchase is this reading's own characterization, yet the arrangement's maintenance has always required measures no purchaser of ordinary property employs: permit and pass regimes over mobility, annuities that deflate toward zero in real terms, pressured reserve surrenders, and statutory control of band governance. Suppression is 0.72, authored as a raw structural property unscaled by power or scope: the arrangement forecloses parallel jurisdiction by law, not by persuasion. Theater_ratio 0.26: the annual-payment and treaty-day apparatus grows increasingly ceremonial, but the land-titles machinery the transaction feeds is fully functional. Accessibility_collapse 0.62 — once the frame is accepted, alternatives such as co-jurisdiction or reopening collapse within it, but the frame itself remains legally contestable, so collapse sits well short of natural-law completeness. Resistance 0.7 — a century of specific-claims litigation, rights cases, blockades, commissions, and international pressure, which the arrangement absorbs procedurally rather than answers. The suppression_requirement series is authored because enforcement-capacity change is a dynamic this story specifically tracks: enforcement machinery intensified through the mid-century permit-and-prohibition era and partially relaxed after constitutional recognition of Aboriginal and treaty rights, without returning to its starting intensity. All three series share one time grid; every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the agenda-setter seat, the arrangement is the settled administration of a concluded purchase: annuities are paid, reserves are held, title flows cleanly — the state experiences order, not extraction. From the payer seat, the same structure operates as confinement: a bounded parcel, a payment that shrinks yearly in real terms, and jurisdiction that stops at the reserve line, with exit unavailable because the nation's collective identity is constituted by its relationship to the territory — relational identity fusion, not professional or ideological lock-in; if that frame broke, the nations would appear as continuing polities rather than counterparties to a closed conveyance, and the entire classification surface would shift. The courts occupy an intermediate seat: they experience the arrangement as doctrine to be applied with honour-of-the-Crown glosses, absorbing drift interpretively rather than confronting it. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Settler governments anchor the beneficiary end: they collect the jurisdiction and dispose of the land, and their arbitrage-grade exit — the ability to reframe the settlement by ordinary legislation — places them nearest the beneficiary pole. Resource and agricultural settlers are derivative beneficiaries with mobile exit: they receive granted title and can divest. Signatory nations anchor the target end: they bear the confinement and the deflating consideration, and identity_locked exit holds them near the full-target pole; their secondary position as recipients of the narrow treaty rights (reserves, annuities, harvesting rights) tempers the derived directionality without reversing it — this is precisely the structural delta the reading declares, nations entering the beneficiary set only for the narrow rights while remaining net payers overall. The excluded parties (Metis and non-status descendants, hereditary structures) bear costs of an arrangement they were never seated in; their powerlessness and trapped or identity-locked exits read as target-side. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already yield the intended spread, and the two institutional seats (governments, courts) are differentiated by role rather than by override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — clearing title so settlement could proceed — was accomplished, and the arrangement persists long past its accomplishment; the parties dispute whether any live problem remains (the state attests that title certainty requires the arrangement's continuous maintenance; the Royal Commission on Aboriginal Peoples and Indigenous scholarship attest the problem was never legitimately the nations' to solve by cession). Status is therefore authored contested, not dead, and the mismatch consumer reads contested paired with world_rearranges as no zombie flag. The tangled_rope classification is what prevents mandatrophy mislabeling in both directions: reading the arrangement as pure extraction erases the real transfers nations litigate to enforce (reserves, annuities, harvesting rights — assets, not theater), while reading it as pure coordination erases the confiscatory asymmetry between continental jurisdiction and unindexed dollars. It is not a piton: gains concentrate demonstrably in the settler-government seat, so theatrical maintenance is a symptom here, not the load-bearing structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the extinguishment_reading of the historical_treaty_substrate kernel; how would the constraint''s structure change under the sibling readings?',
    'Author the sibling stories (stewardship_reading, nation_to_nation_reading) against the same historical referent and compare beneficiary/victim sets and epsilon; the expected deltas are declared in the kernel context.',
    'Under either sibling, signatory nations re-enter the victim set for territorial jurisdiction, the settler state loses sole-legitimate-authority status, and epsilon rises sharply; this file''s low-moderate epsilon is valid only within this reading''s own lights.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame declaration: this story is one reading of a contested kernel, not the topic whole.').

omega_variable(
    cession_premise_location,
    'Where the kernel contest is located: do the written cession and surrender clauses capture what the parties actually agreed, given documented oral undertakings of shared use and the parties'' divergent linguistic records?',
    'Oral-history evidence now admissible in specific-claims and Aboriginal-rights proceedings, weighed against archival negotiation records and contemporaneous commission correspondence.',
    'If the oral sharing-understanding prevails, the conveyance premise fails, this reading collapses toward the stewardship sibling, and epsilon must be re-authored from the new referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_premise_location, empirical, 'The factual hinge on which this reading stands or falls.').

omega_variable(
    reading_internal_extraction_assessment,
    'Does this reading''s own assessment of the arrangement (a concluded purchase being administered, hence modest residual extraction) survive the enforcement record — permit and pass regimes, unindexed annuities, pressured reserve surrenders, statutory control of band governance — that the arrangement''s maintenance actually required?',
    'Compare the bargain''s terms as written against the administrative history compiled in specific-claims decisions and royal-commission findings, asking whether each enforcement measure was contemplated by the transaction.',
    'If most enforcement exceeded the bargain, the reading''s charitable epsilon is unstable and the computed per-seat classifications will diverge from the authored claim — which is the measurement the corpus exists to take.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_internal_extraction_assessment, conceptual, 'Stability of the reading''s internal valuation of the arrangement it legitimates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__extinguishment_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__extinguishment_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__extinguishment_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(hist_tr_t80, historical_treaty_substrate__extinguishment_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__extinguishment_reading, theater_ratio, 100, 0.26).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 60, 0.37).
narrative_ontology:measurement(hist_be_t80, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 80, 0.36).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 60, 0.77).
narrative_ontology:measurement(hist_su_t80, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% Family decomposition of the historical_treaty_substrate kernel: one colloquial label, three structurally distinct constraints. This story (extinguishment_reading) carries the lowest epsilon of the family because its referent is a concluded conveyance assessed on the reading's own terms; the siblings re-seat the parties (nations as continuing sovereigns or co-stewards; the state as one authority among others) and must author higher epsilon over the same historical material. Upstream/downstream: this reading is the currently operative judicial frame, so its persistence shapes the operating environment of both siblings — the edges run from this story to them, mirroring the reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
