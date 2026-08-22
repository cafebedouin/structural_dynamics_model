% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__beneficiary_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__beneficiary_extraction_reading, []).

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
 *   constraint_id: qwerty_persistence_mechanism__beneficiary_extraction_reading
 *   human_readable: Maintained QWERTY Incumbency: Vendor-Protected Standard with Artificial Switching Costs
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   Under this reading, the QWERTY layout persists not as an accident of
 *   timing and not as proof of adequacy, but because identifiable commercial
 *   parties kept it in place. The Remington/Union Typewriter product line and
 *   its manufacturing successors controlled which layouts shipped and
 *   declined to ship rivals long after the original mechanical rationale
 *   lapsed; commercial typing schools built curricula, instructor corps, and
 *   placement pipelines on the standard and defended it; employers hired
 *   against it; and the resulting switching costs were not merely discovered
 *   but manufactured and maintained. The arrangement's extraction runs
 *   through artificial switching costs - equipment-price premiums and
 *   lifetime typing-time losses sustained by keeping layout competition
 *   closed - riding on top of a genuine coordination function (a shared input
 *   convention that lets any operator use any machine). This file
 *   instantiates one reading of the qwerty_persistence_mechanism kernel; the
 *   sibling readings are separate constraint files linked in
 *   network.affects_constraints, and this file's epsilon is authored for this
 *   reading only. KEY AGENTS (by structural relationship): -
 *   qwerty_hardware_incumbents: Primary beneficiary and agenda setter
 *   (institutional/arbitrage) - decides which layouts ship, collects
 *   installed-base rents - commercial_typing_schools: Secondary beneficiary
 *   (organized/constrained) - curriculum and placement investment protected
 *   by the standard - trained_typist_workforce: Dual-positioned
 *   payer-beneficiary (organized/identity_locked) - bears daily inefficiency,
 *   holds protected human capital - alternative_layout_innovators: Excluded
 *   challenger (moderate/trapped) - proposals never shipped, promotion costs
 *   unrecoverable - employing_office_firms: Payer (organized/constrained) -
 *   pays price premiums and absorbs productivity drag -
 *   national_standards_institutes: Analytical observer
 *   (institutional/analytical) - ratifies installed practice, holds the
 *   maintenance record
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.65).
domain_priors:suppression_score(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.55).
domain_priors:theater_ratio(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__beneficiary_extraction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__beneficiary_extraction_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__beneficiary_extraction_reading, "Maintained QWERTY Incumbency: Vendor-Protected Standard with Artificial Switching Costs").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__beneficiary_extraction_reading, "economic_history/technology_studies/path_dependence").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__beneficiary_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__beneficiary_extraction_reading, '4c5db1d0-50ac-4e51-a333-5a2db2010413').
narrative_ontology:cs_kernel_codification('4c5db1d0-50ac-4e51-a333-5a2db2010413', formalized).
narrative_ontology:cs_authority_grounding('4c5db1d0-50ac-4e51-a333-5a2db2010413', extraction).
narrative_ontology:cs_interpretation_layer_present('4c5db1d0-50ac-4e51-a333-5a2db2010413').
narrative_ontology:cs_reading_relation('4c5db1d0-50ac-4e51-a333-5a2db2010413', qwerty_persistence_mechanism__lock_in_reading, influences).
narrative_ontology:cs_reading_relation('4c5db1d0-50ac-4e51-a333-5a2db2010413', qwerty_persistence_mechanism__naturalization_reading, forecloses).
narrative_ontology:cs_axiom('4c5db1d0-50ac-4e51-a333-5a2db2010413', foundational, incumbent_maintenance_causally_necessary).
narrative_ontology:cs_axiom_status(incumbent_maintenance_causally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('4c5db1d0-50ac-4e51-a333-5a2db2010413', incumbent_maintenance_causally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('4c5db1d0-50ac-4e51-a333-5a2db2010413', secondary, switching_costs_above_competitive_baseline).
narrative_ontology:cs_axiom_status(switching_costs_above_competitive_baseline, holdable).
narrative_ontology:cs_axiom_grounding('4c5db1d0-50ac-4e51-a333-5a2db2010413', switching_costs_above_competitive_baseline, empirically_contingent).
narrative_ontology:cs_reference_frame('4c5db1d0-50ac-4e51-a333-5a2db2010413', vendor_governed_standard).
narrative_ontology:cs_drift_state('4c5db1d0-50ac-4e51-a333-5a2db2010413', post_electronic_keyboard_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c5db1d0-50ac-4e51-a333-5a2db2010413', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_hardware_incumbents).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, commercial_typing_schools).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, employing_office_firms).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_typist_workforce).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_typist_workforce).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__beneficiary_extraction_reading, incumbent_maintenance_thesis_of_standard_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, manufactures, and ships typewriters and later computer keyboards, deciding which key layout reaches the market. Inherited the Sholes layout through the Remington and Union Typewriter product lines and declined to ship competing layouts even after the mechanical rationale lapsed, citing tooling, inventory, and customer familiarity. Sets dealer terms and specification defaults. Exit consists of retooling or diversifying product lines, which it controls outright.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_hardware_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).

% Sells keyboard instruction and job placement. Curricula, instructor certification, timing drills, and employer placement agreements are built around the dominant layout. Teaching a different layout would strand course materials and reduce graduates' interview prospects, since employers test applicants on the standard. Exit means rebuilding the school's core product against the hiring market's expectations.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, commercial_typing_schools, beneficiary,
    organized, biographical, constrained, continental).

% Types the standard layout throughout a working career; speed certifications and words-per-minute credentials are layout-specific. The shared standard protects the resale value of that skill, while daily output carries whatever inefficiency the layout has. Retraining to another layout means months of reduced speed and re-certification, and professional self-description - a sixty-word-per-minute typist - is bound to the layout learned first.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_typist_workforce, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, trained_typist_workforce, beneficiary).

% Develops and promotes redesigned keyboards, most prominently the 1936 Dvorak layout. Cannot get major manufacturers to ship hardware, schools to teach courses, or employers to recognize proficiency; promotion campaigns, conversion kits, and studies demonstrating gains failed to change vendor defaults. Proposals reach vendors as unsolicited submissions and die in defaults; sunk promotion costs are unrecoverable.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators, excluded,
    moderate, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(qwerty_persistence_mechanism__beneficiary_extraction_reading, alternative_layout_innovators, payer).

% Buys typewriters and keyboards and hires from the trained pool. Pays equipment prices set with little layout competition and absorbs whatever productivity difference the layout carries. Any single firm requesting an alternative layout faces thin vendor catalogs and an applicant pool tested on the standard, so deviation costs more than conformity; no buyer coalition on layout ever formed.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, employing_office_firms, payer,
    organized, biographical, constrained, global).

% Ratifies keyboard standards and reviews layout proposals on the record. Ratification has followed installed practice rather than leading it, converting vendor defaults into official specifications. Evaluates competing-layout submissions and publishes findings; takes no commercial position and holds an analytical seat over the maintenance dynamic.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__beneficiary_extraction_reading, national_standards_institutes, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_hardware_incumbents).
narrative_ontology:fixing_cost_class(qwerty_persistence_mechanism__beneficiary_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single shared key layout lets any operator use any machine, lets employers hire against a uniform skill test, lets parts, manuals, and training materials interoperate, and lets hardware and later software assume one input convention instead of many.
% TRANSFER_FUNCTION: Moves equipment-price premiums and lifetime typing-time losses from equipment buyers and typists to hardware incumbents, whose installed base and tooling are shielded from layout competition; moves tuition demand to schools aligned with the standard; removes market access from alternative-layout developers.
% ABSENT_VOICES: Alternative-layout developers are outside the room where shipping decisions get made - their proposals arrive as unsolicited submissions and die in vendor defaults. Working typists were never consulted when defaults were set or preserved; their daily time cost enters no vendor calculation. Both would object to layout competition being closed, and neither is present in vendor specification meetings.
% DISAPPEARANCE_RATIONALE: If incumbent maintenance vanished overnight - vendors shipping whichever layouts customers ordered, schools teaching competitively, defaults reopening - hardware catalogs would branch, training would bifurcate, and the installed base's price protection would erode within a product generation. Arrangements across manufacturing, education, and office work depend on the maintained default.
% FOUNDING_PROBLEM: Early Sholes typewriters jammed when adjacent typebars struck in quick succession; the layout credited with separating common letter pairs was Remington's answer to a mechanical jamming problem in 1870s lever machines.
% FOUNDING_PROBLEM_CORROBORATION: Historians of typewriter mechanics and independent engineering analyses - including revisions attributing the layout's evolution to telegraph transcription practice rather than jam spacing - attest that the jamming rationale was the period justification and ceased to bind once typebars gave way to ball, wheel, and electronic printing. No source outside the vendor-and-school beneficiary set attests that the founding problem remains live.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__beneficiary_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__beneficiary_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__beneficiary_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_mechanism__beneficiary_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_mechanism__beneficiary_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.65 because the rents at issue - price premiums on equipment sold into a closed catalog, and the protected value of an installed base no rival layout could reach - are decoupled from any service the incumbents render for them, and are sustained by keeping layout competition shut rather than by participant preference. Suppression is 0.55 at interval end, but the series tells the real story: enforcement rose through the trust era (dealer exclusivity, patent pooling, school-standard alignment, peaking at 0.70 mid-interval when those levers were load-bearing) and then decayed as electronic keyboards and software remapping stripped the technical enforcement levers - extraction persisted on inherited switching costs after fresh coercion stopped being necessary. Theater is 0.25: maintenance was mostly functional (shipping decisions, curricula, defaults), with a growing performative slice (sponsored efficiency studies defending the default) as the mechanical justification thinned. Accessibility_collapse is 0.58: rival layouts remained buildable and demonstrable - conversion kits existed, studies were published - but collapsed practically, since no hardware shipped, no pool of trained operators formed, and no employer recognized proficiency. Resistance is 0.45: sustained advocacy, isolated adoptions, and recurring criticism, but never a buyer coalition. On the multi-victim coalition check: the harmed population was enormous (every typist, every office) but harm was individually small, diffusely attributed, and identity-fused for typists, so coalition power never materialized. Coordination type is information_standard (default floor, no override): the genuine coordination function is a shared input encoding - hardware, software, manuals, and operator skills interoperate on one convention - and nothing in the domain justifies departing from the type default. All three tracked series share one time grid (T=0 approximates the 1880s consolidation under Remington; T=100 approximates the 1980s electronic-keyboard inheritance), so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as product stewardship it built and services it provides; the payer seats experience the same structure as a closed catalog and a bill. The typist seat is internally split in a way the engine should surface: identity_locked exit amplifies target-side exposure (retraining means months of lost speed and re-certification), while the human-capital stake damps it (the standard protects the resale value of the skill). The analytical standards seat sees a maintenance record - ratifications following installed practice - that the vendor seat does not narrate. These divergences are computed from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents sit at the beneficiary end (declared beneficiary, arbitrage exit - they set the terms and can retool at will). Typing schools sit low but not at zero: constrained exit (curricula and placement pipelines stranded by any switch) keeps their exposure nonzero even as beneficiaries. Employing firms sit high (declared payer, constrained exit - deviation costs more than conformity given vendor catalogs and applicant testing). Alternative-layout innovators sit nearest the full-target end (excluded from the conversation, trapped with unrecoverable promotion costs). Trained typists are listed among victims with identity_locked exit, which pushes them targetward, while their secondary beneficiary role damps that - net moderately high, matching their genuinely mixed position. No directionality overrides are used: the beneficiary/victim declarations plus exit options already encode these positions, and the derivation chain handles them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - typebar jamming in 1870s lever machines - died when typebars gave way to balls, wheels, and electronic printing, yet the arrangement persists and the world still rearranges around it. That dead-status-plus-world_rearranges mismatch is the capture/zombie signature: the mandate is gone and something else now holds the structure up. This reading keeps the capturer visible, which is what prevents mislabeling: a pure coordination-failure account would read the persistence as a tragic but blameless rope-like lock-in, and an adequacy account would read it as benign meritocracy, either of which launders the maintenance record. Theater stays low (0.25), so this is not yet a piton - the maintenance was real for most of the interval. But the enforcement-decay-without-rent-decay arc marks the seam where, if rents also erode, the structure drifts toward inertial persistence and the piton questions begin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the beneficiary_extraction_reading of the qwerty_persistence_mechanism kernel: what structural picture would the sibling readings produce if classified independently?',
    'Classify the sibling files separately - qwerty_persistence_mechanism__lock_in_reading (diffuse gains, no capturer, emergent rather than enforced suppression) and qwerty_persistence_mechanism__naturalization_reading (extraction near the coordination floor, no suppression) - and compare epsilon, gain_flow, and fixing_cost across the family.',
    'If the naturalization reading is correct, this file''s epsilon falls toward the coordination floor and the named beneficiaries become mere participants; if the lock-in reading is correct, gains are diffuse with no named capturer and suppression is emergent, dissolving the captured-receipt profile this reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of the QWERTY-persistence kernel; siblings relocate agency and the location of gains.').

omega_variable(
    maintenance_evidence_attribution,
    'Does archival evidence show deliberate maintenance - refusals to license or ship rival layouts, coordinated dealer terms, school-standard alignment - as opposed to passive non-adoption?',
    'Manufacturer correspondence, patent-pool and licensing records, dealer contract archives, and commercial-school accreditation history spanning the 1890s-1950s.',
    'Documented active maintenance confirms the enforcement structure this reading asserts; purely passive non-adoption would collapse this reading into the lock-in sibling and remove the named-capturer receipt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_evidence_attribution, empirical, 'Whether suppression was enacted by identifiable agents or emerged without agency.').

omega_variable(
    dvorak_superiority_dispute,
    'How large is the real efficiency gap between the maintained layout and the suppressed alternatives, given that headline superiority studies (notably the 1940s Navy trial) were conducted under Dvorak''s own direction and later re-analysis found smaller or negligible gaps?',
    'Sponsor-independent blind trials on matched cohorts; meta-analysis of the layout-comparison literature separating sponsored from independent studies.',
    'If the gap is negligible, the victim-side harm of the artificial switching costs shrinks toward pure rent transfer without efficiency loss, lowering measured extraction; if substantial, the maintained inefficiency is a large continuing levy on every typist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_superiority_dispute, empirical, 'Size of the efficiency loss the maintained standard imposes, given contested superiority evidence.').

omega_variable(
    typist_net_position,
    'Are trained typists net gainers (the shared standard protects the scarcity value of their certified skill) or net losers (lifetime inefficiency plus retraining exposure)?',
    'Wage and productivity analysis comparing layout-typed compensation against counterfactual-output estimates, plus transition-cost studies from the few recorded mass retrainings.',
    'A net-gainer finding moves this seat toward the beneficiary end of directionality and softens the victim structure; a net-loser finding strengthens the extraction reading''s victim side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(typist_net_position, empirical, 'Dual-positioned seat: human-capital protection versus lifetime efficiency cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__beneficiary_extraction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(qwer_tr_t0, observed).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(qwer_tr_t20, observed).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(qwer_tr_t40, observed).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement_basis(qwer_tr_t60, observed).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 80, 0.23).
narrative_ontology:measurement_basis(qwer_tr_t80, observed).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement_basis(qwer_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(qwer_be_t0, observed).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(qwer_be_t20, observed).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(qwer_be_t40, observed).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(qwer_be_t60, observed).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement_basis(qwer_be_t80, observed).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, base_extractiveness, 100, 0.65).
narrative_ontology:measurement_basis(qwer_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(qwer_su_t0, observed).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(qwer_su_t20, observed).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(qwer_su_t40, observed).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(qwer_su_t60, observed).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(qwer_su_t80, observed).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_mechanism__beneficiary_extraction_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement_basis(qwer_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__beneficiary_extraction_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__beneficiary_extraction_reading, qwerty_persistence_mechanism__naturalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'why QWERTY persists' covers three structurally distinct claims with different epsilon over the same standing arrangement (maintained QWERTY incumbency). This file (beneficiary_extraction_reading) authors epsilon 0.65 for a maintained arrangement with a named capturer and enforced suppression. qwerty_persistence_mechanism__lock_in_reading authors a coordination-failure account: diffuse gains, no capturer, suppression emergent rather than enforced. qwerty_persistence_mechanism__naturalization_reading authors an adequacy account: epsilon near the coordination floor, alternatives lapsed fairly. Each file keeps a single stable epsilon assessed by its own lights; the files form a constraint family linked here rather than one story averaged across observables. Upstream/downstream: the naturalization account is the incumbents' public framing and is cited as evidence against the extraction account; the extraction account, where corroborated, drains the inevitability framing the lock-in account relies on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
