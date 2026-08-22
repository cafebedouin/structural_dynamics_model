% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness as Memorial Performance — Husk Reading of the Preparedness-Commitment Kernel
 *   domain: institutional/disaster-preparedness/commitment-systems
 *
 * SUMMARY:
 *   A mandatory inter-agency drill and exercise regime, founded after
 *   catastrophic coordination failures, has — on this reading — matured into
 *   memorial performance. The exercises run on schedule, checklists certify,
 *   after-action reports file, and the annual showcase draws officials and
 *   cameras; but the scenario repertoire is frozen, no element of the routine
 *   transmits operational capacity under novel stress, and the regime's
 *   persistence is sustained by inertia and commemoration rather than by
 *   anyone's concentrated gain. Agencies administer it for mandate
 *   continuity, vendors supply it for contract revenue, officials attend it
 *   for credit; none profits enough to defend it against reform, and none is
 *   hurt enough in the standing state to force one. The epsilon referent is
 *   this standing arrangement as the husk reading assesses it: high
 *   form-compliance, low adaptive capacity. This file is one reading of the
 *   preparedness_commitment kernel; see kernel_context and the committer
 *   omegas for the reading structure. KEY AGENTS (by structural
 *   relationship): emergency_management_agencies — agenda-setter
 *   (institutional/constrained); drill_training_vendors — beneficiary
 *   (moderate/mobile); elected_officials — beneficiary (powerful/mobile);
 *   frontline_responders — primary payer (organized/constrained);
 *   at_risk_public — ultimate payer (powerless/trapped);
 *   community_resilience_organizers — excluded (moderate/mobile);
 *   disaster_research_community — analytical observer
 *   (analytical/analytical).
 *
 * KEY AGENTS:
 *   - emergency_management_agencies: agenda-setter (institutional/constrained) — administers the drill calendar and compliance metrics; could redesign toward competency assessment but bears the admission cost
 *   - drill_training_vendors: beneficiary (moderate/mobile) — sells scenario packages and certifications calibrated to the compliance checklist; thin, contract-cycle gain
 *   - elected_officials: beneficiary (powerful/mobile) — collects reputational credit from exercise attendance and drill-count testimony; bears no operational exposure
 *   - frontline_responders: primary payer (organized/constrained) — mandatory hours on a frozen scenario repertoire crowd out genuine skill maintenance
 *   - at_risk_public: ultimate payer (powerless/trapped) — receives manufactured reassurance and bears the unpriced tail cost when novel stress meets a hollowed response
 *   - community_resilience_organizers: excluded (moderate/mobile) — propose realistic local capacity-building; no seat in exercise design
 *   - disaster_research_community: analytical observer (analytical/analytical) — documents the gap between exercise metrics and operational outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.52).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.38).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance — Husk Reading of the Preparedness-Commitment Kernel").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/disaster-preparedness/commitment-systems").

domain_priors:requires_active_enforcement(preparedness_commitment__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'fe505501-307b-4221-bbdb-2f4f877fc344').
narrative_ontology:cs_kernel_codification('fe505501-307b-4221-bbdb-2f4f877fc344', formalized).
narrative_ontology:cs_authority_grounding('fe505501-307b-4221-bbdb-2f4f877fc344', expertise).
narrative_ontology:cs_interpretation_layer_present('fe505501-307b-4221-bbdb-2f4f877fc344').
narrative_ontology:cs_reading_relation('fe505501-307b-4221-bbdb-2f4f877fc344', preparedness_commitment__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('fe505501-307b-4221-bbdb-2f4f877fc344', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('fe505501-307b-4221-bbdb-2f4f877fc344', foundational, form_compliance_is_not_retention).
narrative_ontology:cs_axiom_status(form_compliance_is_not_retention, holdable).
narrative_ontology:cs_axiom_grounding('fe505501-307b-4221-bbdb-2f4f877fc344', form_compliance_is_not_retention, empirically_contingent).
narrative_ontology:cs_axiom('fe505501-307b-4221-bbdb-2f4f877fc344', secondary, commemoration_does_not_substitute_for_training).
narrative_ontology:cs_axiom_status(commemoration_does_not_substitute_for_training, holdable).
narrative_ontology:cs_axiom_grounding('fe505501-307b-4221-bbdb-2f4f877fc344', commemoration_does_not_substitute_for_training, instrumental).
narrative_ontology:cs_reference_frame('fe505501-307b-4221-bbdb-2f4f877fc344', founding_competence_regime).
narrative_ontology:cs_drift_state('fe505501-307b-4221-bbdb-2f4f877fc344', contemporary_compliance_mature_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe505501-307b-4221-bbdb-2f4f877fc344', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, emergency_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, drill_training_vendors).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, elected_officials).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, at_risk_public).
narrative_ontology:constraint_vindicates(preparedness_commitment__husk_reading, exercise_count_as_readiness_evidence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the drill calendar, certify exercise completion, and file the compliance reports that funding formulas and oversight hearings consume; staff advancement tracks exercise counts and closed audit findings. The agencies could redesign the regime toward competency assessment with realistic, novel-scenario testing, but the redesign requires declaring current exercises deficient, re-procuring vendor contracts, and rewriting the metrics their budgets ride on. Exiting administration of the regime is not available: the mandate is statutory and they are its named administrator.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, emergency_management_agencies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_commitment__husk_reading, emergency_management_agencies, beneficiary).

% Sell scenario packages, evaluation rubrics, inject scripts, and certification services sized to the compliance checklist; revenue renews on the exercise cycle. A shift to outcome-based competency contracting would require rebuilding the product line around measurable skill transfer. Taking the same services to other public-sector clients is straightforward if the regime's procurement changes.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, drill_training_vendors, beneficiary,
    moderate, immediate, mobile, national).

% Open the annual showcase exercise, cite drill counts in oversight hearings, and appear in the commemorative photo cycle with responders. The credit accrues within an election cycle while the operational consequences of readiness gaps arrive on a longer clock than any term. They can redirect attention at will and bear no direct operational exposure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, elected_officials, beneficiary,
    powerful, immediate, mobile, national).

% Spend mandatory hours each cycle walking scenarios whose choreography has not changed in years — casualties staged at the same corners, triage at the same stations, the same after-action forms. Many privately rate the scenarios unrealistic and maintain real skill off the clock. Attendance is tracked and feeds the metrics their agencies report; opting out carries career consequence, and leaving the profession carries pension and identity costs.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    organized, biographical, constrained, regional).

% Live in the hazard zones the regime covers and receive its output as reassurance: published preparedness scores, exercise coverage in local media, anniversary ceremonies for past disasters. Exiting the hazard exposure is not cheaply available, and no channel exists for testing whether the reassurance tracks capability. When a disaster arrives outside the rehearsed repertoire, they bear the gap between the rehearsed image and the delivered response.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, at_risk_public, payer,
    powerless, generational, trapped, regional).

% Build neighborhood-level capacity — equipment caches, skills courses, realistic local exercises on actual terrain with volunteers — and have proposed folding this capacity into the official cycle. Exercise design is run by agencies and vendors around compliance metrics; the organizers hold no seat in that process, and their proposals surface, when at all, as annexes rather than design inputs. They operate outside the regime and can continue regardless of what the regime does.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, community_resilience_organizers, excluded,
    moderate, biographical, mobile, local).

% Compile exercise records alongside post-incident performance data and publish comparisons of drilled versus undrilled task outcomes. Findings circulate in journals and after-action literature but enter the regime's design process only through discretionary citation. They hold no administrative seat and bear none of the regime's costs.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, disaster_research_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime coordinates many organizations onto one rehearsal schedule, one command vocabulary, and one set of exercise interfaces, so that agencies that rarely operate together share procedures and language in an incident; it also coordinates attention, giving agencies, officials, vendors, and the public a shared annual date on which preparedness is performed and noticed.
% TRANSFER_FUNCTION: Moves responder hours, agency training budgets, and public attention into the exercise apparatus: budget dollars flow to vendors as contract payments, exercise counts flow to agencies as budget justification and to officials as credit, and reassurance flows to the public — paid for by responder time and by public exposure to the difference between the rehearsed image and the delivered response.
% ABSENT_VOICES: Competency-based training advocates and community resilience organizers are absent from exercise design, which agencies and vendors run around compliance metrics. Responders are present as participants, but no channel carries their private assessment that the scenarios are unrealistic into design. The at-risk public appears as staged casualties, never as principals with standing to question what the exercises certify.
% DISAPPEARANCE_RATIONALE: Budgets keyed to exercise counts would lose their justification instrument, vendor contracts would strand, agency metrics and oversight hearings would lose their evidence base, and the public would lose the reassurance layer — the institutional arrangements would reorganize within a budget cycle. On this reading no operational capacity would be lost with it, because the routines transmit form rather than function: the rearrangement is institutional, not operational.
% FOUNDING_PROBLEM: After catastrophic disasters exposed agencies that had never rehearsed together — incompatible command vocabularies, untested plans, interfaces that failed on first contact — governments mandated a joint exercise regime to force rehearsal, standardize command language, and convert lessons into revised plans.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: after-action reviews of recent major incidents, compiled by external review boards and academic incident databases, attest that joint operations now run on shared command vocabulary and rehearsed interfaces — the founding coordination failure no longer appears as the operative failure mode — while locating current failures in scenario classes outside the drilled repertoire. The regime's beneficiaries attest the founding problem is live, citing evolving threats; that attestation tracks the budget cycle and is not corroborated by operational outcome data.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater_ratio 0.78 is the husk signature: the dominant share of regime activity — scheduled drills against a frozen repertoire, documentation and certification cycles, the annual showcase — produces compliance artifacts rather than tested capacity; the residual functional share is narrow onboarding (new staff absorb the command vocabulary). Extractiveness 0.52 is moderate: the standing arrangement taxes responder hours and training budgets and manufactures public reassurance, while its heaviest cost is contingent and tail-loaded, landing only when a novel-stress disaster meets a hollowed response — base epsilon prices that partially. Suppression 0.38 is low-to-moderate and procedural: attendance is mandated and careers run on compliance metrics, but the regime rarely coerces hard because it rarely needs to — the ritual is comfortable and its costs stay invisible until they are catastrophic; the public-side lock is epistemic rather than coercive (no channel exists to test the reassurance), carried in the at_risk_public situation rather than the suppression scalar. Accessibility_collapse 0.42: competency-based alternatives remain visible and articulated — reform proposals, after-action recommendations — but do not displace the regime, because adoption requires coordinated admission of hollowness across standards bodies, funding formulas, and political principals. Resistance 0.28 is low: responders grumble, researchers document, organizers propose, and no seat bears enough standing cost to sustain opposition. The measurement series run on one shared time grid — every tracked metric authored at every point — showing monotonic drift, not oscillation: the regime's activity is annual-cyclical, but each showcase cycle functions as intermittent reinforcement that re-manufactures reassurance, so the metric trajectory rises steadily. Boltzmann coordination type is identity_coordination: what the regime still demonstrably coordinates is commitment and membership — the profession's shared identity, the public's trust, the annual date — which is precisely the layer this reading says survives after the competence function atrophied; the default floor stands, and the identity framing here is the diagnosis, not a cover story. Claimed type piton and the metrics are independent authorings; the engine computes each seat's type from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently. From the agencies' seat the regime is a commitment system administered with professional diligence: the drill calendar is evidence of seriousness, and the competence question stays abstract until an external review names it. From the responders' seat the same calendar is mandatory hours on scenarios everyone privately rates unrealistic — extraction experienced as time and skill atrophy. From the officials' seat it is credit without exposure; from the public's seat it is invisible except as reassurance, with costs arriving only as surprise. Same-level differentiation: drill_training_vendors and community_resilience_organizers hold the same moderate power with the same mobile exit, but the vendors sit inside the regime's revenue loop while the organizers sit outside its design conversation entirely — position, not power, differentiates them. Inter-institutionally, the research community's findings circulate freely yet enter the regime only through discretionary citation: the analytical seat sees the full form-function gap that the administering seat's metrics are built not to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: vendors (contract revenue), officials (reputational credit), and the agencies (mandate continuity — declared beneficiary, but near-symmetric; see the override). Payers: responders (time, crowding out of genuine skill maintenance) and the at-risk public (manufactured reassurance, unpriced tail exposure). The derivation maps declared beneficiaries to low directionality and declared payers to high. The institutional override corrects the agencies to roughly symmetric (0.45): they also administer the regime, absorb its audit burden, and own the reputational exposure when the form-function gap surfaces, so their beneficiary declaration alone would understate their cost-bearing; only one stakeholder holds the institutional atom, so the override is unambiguous. No overrides are needed elsewhere: vendors and officials sit near the beneficiary end, responders and the public near the target end. Suppression is authored as a raw structural property and enters the engine unscaled; only extractiveness is scaled by directionality and spatial scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — agencies that had never rehearsed together, failing on incompatible vocabularies and untested plans — was real and was solved: joint operations now run on shared command language and rehearsed interfaces, as external after-action records attest. What persists is the rehearsal form without the capacity it was built to carry. The founding_problem_status by disappearance_verdict pair (dead by world_rearranges) is the zombie signature: budgets, vendor contracts, metrics, and public confidence still depend on the regime, so the institutional world rearranges around its removal even though, on this reading, no operational capacity would be lost with it. The piton classification blocks both symmetric mislabels: reading the regime as rope would excuse the hollow core on the strength of a coordination shell that no longer carries function; reading it as snare would require a concentrated capturer actively suppressing exits, and none exists — the gains are too thin and diffuse for any seat to bother defending the regime against reform, which is why resistance stays low without suppression doing the work. Receipt surface check: each seat's gain was examined — vendor contract margin, agency budget continuity, official credit — and none constitutes capture of the extracted hours and exposure, which are consumed and borne rather than received by any seat; gain_flow is therefore authored as diffuse. Fixing is prohibitive for the seat that could fix it (the agencies): redesign requires declaring current exercises deficient, re-procuring contracts, and rewriting the metrics their budgets ride on, costs that exceed what the status quo charges them. The classification locates the reform lever accordingly: this is an inertia equilibrium, not a predator, and it breaks only when a documented competence collapse makes the status quo's cost legible to the seat that could fix it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_reading_of_preparedness_kernel,
    'This story instantiates the husk_reading of the preparedness_commitment kernel: the standing drill-and-exercise regime is memorial performance that feels like retention but lacks operational competence. Read from the sibling readings — competence_reading (the same routines maintain live exercised knowledge across generations) or hybrid_reading (memorial elements stabilize commitment while competence elements maintain function) — the same regime would carry a different beneficiary structure, theater profile, and epsilon.',
    'Author the sibling readings as separate constraint stories with independent epsilon, beneficiaries, and claimed types over the same referent; compare classifications through the kernel''s reading_relations rather than inside this file.',
    'If the competence_reading is structurally right, this story''s theater_ratio and extractiveness overstate the regime''s hollowness and its computed type moves toward rope; if the hybrid_reading is right, part of the measured theater is commitment-stabilization cost rather than dead loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_reading_of_preparedness_kernel, conceptual, 'Committer structure: this file is the husk_reading of the preparedness_commitment kernel; sibling readings are separate constraints.').

omega_variable(
    drill_form_capacity_gap,
    'The readings divide on one structural element: does drill activity transmit operational capacity under novel stress, or only the form of preparedness? Which side does outcome evidence favor for the current regime?',
    'Paired exercises with blinded evaluators scoring transfer to novel scenarios, plus post-incident audits comparing performance on drilled versus undrilled task classes.',
    'Demonstrated transfer collapses this story''s theater_ratio toward the competence_reading''s profile and forces reclassification away from the piton claim; demonstrated non-transfer confirms the husk reading and hardens the dead-founding-problem signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_form_capacity_gap, empirical, 'Whether the regime''s routines carry live capacity or only compliance form — the structural locus of the kernel contest.').

omega_variable(
    novel_stress_competence_collapse,
    'Under what class of novel stress does the regime''s adaptive-capacity deficit manifest as response collapse, and has a documented event already occurred that after-action analysis attributes to rehearsal hollowness rather than resource shortfall?',
    'Systematic coding of post-incident reports for responses whose scenario class fell outside the drilled repertoire, separating equipment and funding failures from coordination and skill failures.',
    'A documented collapse event confirms this reading''s predicted failure mode, raises resistance, and could break the fixing-cost deadlock by making the status quo''s cost legible to the seat that could fix it; repeated absence weakens the husk claim toward the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(novel_stress_competence_collapse, empirical, 'The predicted competence-collapse-under-novel-stress failure mode and whether the record already contains it.').

omega_variable(
    commemorative_function_value,
    'Does the memorial layer of the regime perform a genuinely valuable function — honoring past casualties, sustaining institutional commitment and public attention — with standing independent of operational competence?',
    'Comparative analysis of jurisdictions that separated commemoration (memorials, anniversaries, ceremonies) from training (competency exercises): did commitment and attention metrics hold while training outcomes improved?',
    'If the commemorative function is valuable and separable, part of this story''s extractiveness is mispriced and the honest structure approaches the hybrid reading; if inseparable, the memorial performance is load-bearing for the hollowness itself and the husk reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_function_value, conceptual, 'Whether the memorial layer has independent value or is the carrier of the hollowness.').

omega_variable(
    authority_grounding_framing_ambiguity,
    'Is the regime''s authority grounded in expertise (credentialed professional bodies adjudicating preparedness standards) or in lineage (continuity with the post-disaster founding reforms, sustained commemoratively)? The framings yield different drift readings: under lineage the gap is codification_collapse of transmitted founding practice; under expertise it is practice_drift of a professional standard.',
    'Trace the warrant chain in current exercise standards and certification criteria: do they cite operational outcome evidence (expertise warrant) or founding-reform continuity and anniversary practice (lineage warrant)?',
    'Under the lineage framing the authority''s legitimacy rests on commemorative continuity, which renders the husk diagnosis self-sealing; drift magnitude re-rates toward severe and the reform lever shifts from standards bodies to political principals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing_ambiguity, conceptual, 'CS framing under-determination: expertise versus lineage grounding for the regime''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t6, preparedness_commitment__husk_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement_basis(prep_tr_t6, observed).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__husk_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement_basis(prep_tr_t12, observed).
narrative_ontology:measurement(prep_tr_t18, preparedness_commitment__husk_reading, theater_ratio, 18, 0.64).
narrative_ontology:measurement_basis(prep_tr_t18, observed).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.72).
narrative_ontology:measurement_basis(prep_tr_t24, observed).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement_basis(prep_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t6, preparedness_commitment__husk_reading, base_extractiveness, 6, 0.41).
narrative_ontology:measurement_basis(prep_be_t6, observed).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__husk_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement_basis(prep_be_t12, observed).
narrative_ontology:measurement(prep_be_t18, preparedness_commitment__husk_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement_basis(prep_be_t18, observed).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(prep_be_t24, observed).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__husk_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(prep_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is the husk_reading member of the preparedness_commitment kernel family. The kernel decomposes into three readings — competence_reading (routines maintain live exercised knowledge), husk_reading (this file: routines are memorial performance lacking operational competence), and hybrid_reading (memorial elements stabilize commitment while competence elements maintain function). The readings share one referent — the standing drill-and-exercise regime — and differ in epsilon and structure because they locate function differently; each is authored as a separate, epsilon-invariant constraint and linked here. The competence_reading makes the strong continuity claim; this reading and the hybrid reading both concede the memorial layer and contest the competence layer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__husk_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
