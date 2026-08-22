% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__moderate_iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__moderate_iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__moderate_iconoclast_reading
 *   human_readable: Graduated Image Code under Ecclesial Gatekeeping (Moderate Iconoclast Reading)
 *   domain: theology/religious authority/visual culture
 *
 * SUMMARY:
 *   An ecclesial regulatory authority administers a graduated image code:
 *   three-dimensional statuary is prohibited outright and subject to
 *   confiscation and destruction, while two-dimensional devotional images are
 *   permitted only under license, prescribed subject limits, workshop
 *   inspection, and fee payment. The code is justified as calibrating
 *   prohibition to idolatry risk — flat images, on this account, carry a
 *   manageable danger that oversight can contain. Operationally the
 *   arrangement sustains a permanent administrative apparatus: application
 *   review, subject-approval boards, periodic inspection of workshops and
 *   parish collections, penalty schedules, and a licensed producer class
 *   whose market position the code protects. Compliance costs fall on laity
 *   seeking images, on artisans producing them, and overwhelmingly on the
 *   statuary trade, whose entire output is criminalized. The epsilon referent
 *   is this standing regulated-image regime as assessed by the reading's own
 *   lights — the regime under contest, not any deregulated alternative.
 *   Claimed type and metrics are authored independently: the claim states
 *   snare; the metrics describe the regime's observed operation.
 *
 * KEY AGENTS:
 *   - religious_regulatory_authority: agenda-setter (institutional/arbitrage) — writes the code, licenses, inspects, collects fees and fines, orders destruction
 *   - devout_laity: primary payer among the governed (powerless/identity_locked) — bears fees, inspections, and the loss of statuary devotion; bound by faith membership
 *   - statuary_craftspeople: concentrated victim (moderate/constrained) — trade criminalized, stock destroyed, capital expropriated
 *   - licensed_image_makers: secondary beneficiary (organized/mobile) — protected market created by the licensing barrier
 *   - monastic_communities: payer and custodian of suppressed practice (organized/identity_locked)
 *   - independent_artisans: payer outside the licensed class (moderate/constrained)
 *   - iconodule_sympathizers: excluded voice (organized/trapped) — would object but barred from code-setting councils
 *   - theological_analysts: analytical observer (analytical/analytical) — documents the gap between stated rationale and operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72).
domain_priors:suppression_score(decalogue_image_prohibition__moderate_iconoclast_reading, 0.78).
domain_priors:theater_ratio(decalogue_image_prohibition__moderate_iconoclast_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(decalogue_image_prohibition__moderate_iconoclast_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__moderate_iconoclast_reading, snare).
narrative_ontology:human_readable(decalogue_image_prohibition__moderate_iconoclast_reading, "Graduated Image Code under Ecclesial Gatekeeping (Moderate Iconoclast Reading)").
narrative_ontology:topic_domain(decalogue_image_prohibition__moderate_iconoclast_reading, "theology/religious authority/visual culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__moderate_iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__moderate_iconoclast_reading, '0f02218f-5712-4ec2-a7e8-dafdc416b057').
narrative_ontology:cs_kernel_codification('0f02218f-5712-4ec2-a7e8-dafdc416b057', fixed_text).
narrative_ontology:cs_authority_grounding('0f02218f-5712-4ec2-a7e8-dafdc416b057', extraction).
narrative_ontology:cs_interpretation_layer_present('0f02218f-5712-4ec2-a7e8-dafdc416b057').
narrative_ontology:cs_reading_relation('0f02218f-5712-4ec2-a7e8-dafdc416b057', decalogue_image_prohibition__iconoclast_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f02218f-5712-4ec2-a7e8-dafdc416b057', decalogue_image_prohibition__iconodule_reading, coexists_with).
narrative_ontology:cs_axiom('0f02218f-5712-4ec2-a7e8-dafdc416b057', foundational, decalogue_prohibits_graven_statuary).
narrative_ontology:cs_axiom_status(decalogue_prohibits_graven_statuary, holdable).
narrative_ontology:cs_axiom_grounding('0f02218f-5712-4ec2-a7e8-dafdc416b057', decalogue_prohibits_graven_statuary, theological).
narrative_ontology:cs_axiom('0f02218f-5712-4ec2-a7e8-dafdc416b057', secondary, flat_images_regulable_without_violation).
narrative_ontology:cs_axiom_status(flat_images_regulable_without_violation, holdable).
narrative_ontology:cs_axiom_grounding('0f02218f-5712-4ec2-a7e8-dafdc416b057', flat_images_regulable_without_violation, empirically_contingent).
narrative_ontology:cs_reference_frame('0f02218f-5712-4ec2-a7e8-dafdc416b057', risk_graduated_material_mediation_ban).
narrative_ontology:cs_drift_state('0f02218f-5712-4ec2-a7e8-dafdc416b057', late_regulatory_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0f02218f-5712-4ec2-a7e8-dafdc416b057', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__moderate_iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_image_makers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, statuary_craftspeople).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__moderate_iconoclast_reading, independent_artisans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and revises the image code, convenes the subject-approval boards, licenses permitted flat images, inspects workshops and parish collections, orders confiscation and destruction of statuary, and collects license fees, inspection charges, fines, and forfeited property. Its interpretive jurisdiction grows with every new category the code requires it to police, and it can redefine what counts as a regulated image because it answers to no external reviewer.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Seek sanctioned devotional images for household and parish use. They submit applications, pay fees, submit to inspections of their collections, and have permanently lost the carved devotional forms previous generations used. Their faith membership is constitutive of their lives, so leaving the community to escape the fees and inspections is not a live option; compliance is experienced as piety even where it functions as payment.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity, payer,
    powerless, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__moderate_iconoclast_reading, devout_laity, beneficiary).

% Hereditary guild workers whose entire product line is criminalized. Workshops are seized or forcibly converted, finished stock is destroyed, and tools are confiscated. They can retrain for flat work at a fraction of their former earnings, take secular commissions at a discount, or emigrate to jurisdictions without the code; none of these preserves the trade or the accumulated capital.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, statuary_craftspeople, payer,
    moderate, biographical, constrained, regional).

% Produce two-dimensional devotional images outside the licensed class. They face recurring compliance review, fee schedules scaled to output, and seizure risk for any subject the boards decline to approve. Shifting wholly to secular markets is possible but costs them their specialty and their principal customer base.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, independent_artisans, payer,
    moderate, biographical, constrained, national).

% Hold exclusive licenses to produce approved devotional imagery. The licensing barrier suppresses unlicensed competition and guarantees demand from applicants who cannot obtain images elsewhere. They contribute to the boards' operating costs and support the inspection apparatus that protects their market position.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, licensed_image_makers, beneficiary,
    organized, generational, mobile, continental).

% Custodians of older devotional practice, including carved and sculpted works accumulated over centuries. They must surrender or destroy statuary in their keeping, absorb the cost of converting chapels to compliant decoration, and host inspections of their collections. Their vows bind them to the community that enforces the code, and their archives make them conspicuous targets for enforcement.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, monastic_communities, payer,
    organized, civilizational, identity_locked, continental).

% Clergy, monastics, and literate laity who hold that honor rendered through images is legitimate devotion rather than idolatry. They are barred from the councils that set and revise the code, circulate arguments at personal risk, and shelter non-compliant works. Seated at the code-setting table, they would dismantle the licensing regime rather than reform it.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_sympathizers, excluded,
    organized, generational, trapped, continental).

% Comparative scholars, canon lawyers, and historians examining how the code operates against its stated rationale. They hold no enforcement stake, collect testimony from every seat, and document the relationship between the code's risk-calibration language and its fee, licensing, and confiscation practice.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__moderate_iconoclast_reading, theological_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__moderate_iconoclast_reading, religious_regulatory_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__moderate_iconoclast_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a uniform, teachable standard for what material mediation of the sacred is permissible across scattered congregations: it solves a real boundary-maintenance problem by drawing one line (statuary out, regulated flat images in) instead of leaving each community to negotiate the idolatry question alone.
% TRANSFER_FUNCTION: Moves license fees, inspection charges, fines, and confiscated statuary from laity, artisans, and monastic houses to the regulatory authority; moves protected market share to licensed image makers; and moves definitional authority over the sacred upward to the boards.
% ABSENT_VOICES: Iconodule sympathizers and the statuary guilds would object that the code criminalizes legitimate devotion and expropriates a lawful trade, but they are barred from the councils where the code is drafted and revised; their objections survive only as contraband pamphlets and petition records.
% DISAPPEARANCE_RATIONALE: If the code and its enforcement vanished overnight, statuary production would resume within a generation, the licensed producers' rents would evaporate, the inspection boards and fee schedules would dissolve for want of purpose, and devotional practice would re-expand into the forms the code suppressed. The authority's visual-culture jurisdiction is an arrangement sustained by machinery, not a fact, and its removal reorganizes both the religious economy and the practice it polices.
% FOUNDING_PROBLEM: Protecting a monotheist community from reabsorption into the surrounding culture of cultic statuary, where carved images of the divine were the ordinary instrument of worship.
% FOUNDING_PROBLEM_CORROBORATION: Lay petitions corroborate that fear of idolatrous relapse is sincerely held among the governed, not merely asserted by the authority, and settlement-era chronicles corroborate that the original threat environment was real. But no source outside the benefiting parties attests that the present fee-and-inspection apparatus is proportionate to the residual risk: guild records and dissenting clergy testimony attest that enforcement intensity tracks revenue and jurisdiction, and external chroniclers note that the cultic environment the code was built against no longer exists in its founding form. Corroboration of the founding problem: partial and aging; corroboration of the apparatus's present necessity: none outside the beneficiary set.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__moderate_iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__moderate_iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__moderate_iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__moderate_iconoclast_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__moderate_iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__moderate_iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72): the code converts a doctrinal boundary into a revenue and control stream — license fees, inspection charges, fines, and forfeited statuary flow to the authority, and the permitted channel is narrow enough that access itself becomes a priced favor. Suppression is higher (0.78) because persistence depends on active machinery: destruction orders, workshop seizures, penalty schedules, and the criminalization of an entire craft; alternatives are not argued away but physically removed. Theater ratio is moderate (0.30): subject review and inspection do screen for the abuse the code names, but a growing share of activity is ritualized compliance performance — renewal stamps, inventory filings, ceremonial burnings — that measures obedience more than it reduces idolatry risk. Accessibility collapse is mid-range (0.55): the flat-image channel keeps a lawful alternative open, so the constraint does not close every exit, but the statuary alternative is fully shut and its reopening is unthinkable within the framework. Resistance is substantial (0.60): smuggled carvings, dissenting clergy, patron evasion, and open sympathy for the suppressed practice persist despite penalties. All three tracked series run on one shared grid (t=0 to 48, step 8) so no metric borrows another's end-state values at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   From the authority seat the code is faithful stewardship: a dangerous practice contained at exactly the point the commandment's protection requires, administered with due process and recorded diligence. From the payer seats the same structure reads as toll-taking: the faithful must buy back, piece by piece, practices their tradition once exercised freely, and the inspectors' discretion is effectively the law. Licensed makers see a franchise; monastics see confiscation of inherited sanctity; the excluded see a usurpation. The engine computes these divergent per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The authority sits at the beneficiary pole (d near 0.0): it collects fees, fines, and forfeited property, and its jurisdiction expands with every category it polices. Licensed image makers sit near it (d roughly 0.15-0.25): the licensing barrier is a rent they collect without administering the regime. Statuary craftspeople, monastic communities, and independent artisans sit at the target pole (d roughly 0.85-1.0): they pay in destroyed capital, compliance labor, and suppressed practice, with constrained or identity-locked exit amplifying their exposure. Devout laity carry a declared secondary beneficiary role — they do receive lawful devotional images — which would derive a near-symmetric d; the override to 0.85 corrects this because the regime's net effect on them is extraction: they lost the statuary register of devotion entirely, pay for what remains, and cannot exit their faith to escape the fees. Continental scope scales verification difficulty upward, amplifying effective extraction for the paying seats. Suppression is authored as a raw structural property and is deliberately left unscaled; only extractiveness rides the directionality and scope modifiers.
 *
 * MANDATROPHY ANALYSIS:
 *   The regime's founding problem — protecting the community from reabsorption into surrounding cultic statuary practice — was once a live coordination problem, and in that early phase the arrangement sat closer to a rope carrying real enforcement costs. The measurement series show the characteristic drift: extractiveness and suppression ratcheting upward across the interval while the threat environment that justified them receded, with theater rising as inspection became routine. Reading the arrangement as pure coordination would miss the asymmetric extraction now visible in the receipt flows; reading it as extraction with no residual function would miss the genuine screening work subject review still performs. The snare claim marks the current balance: enforcement-dependent, victim-bearing, with the coordination story increasingly serving as cover. If the founding problem's death becomes settled rather than contested, the terminal paths diverge — enforcement decay yields a piton of theatrical inspection, while a continued ratchet yields deepening snare; mandatrophy is declared only on that settled death, not on the present contested state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the decalogue_image_prohibition kernel — is the graduated-scope reading the correct instantiation, or do the strict (iconoclast_reading) and worship-targeted (iconodule_reading) readings instantiate the true constraint?',
    'Not resolvable from inside any single reading: each reading fixes its own epsilon, victim set, and type, and adoption of a reading is antecedent to measurement. The corpus resolves it structurally by modeling each reading as its own constraint file linked through network edges.',
    'All metrics and classifications in this file are indexed to the moderate_iconoclast_reading; comparing them against sibling files compares different constraints, not measurement error on one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity of the constraint within the contested kernel.').

omega_variable(
    sibling_scope_line_disagreement,
    'Where the readings disagree is located at the scope line of the prohibition: all material mediation (iconoclast_reading), three-dimensional statuary only (this reading), or acts of image-worship only (iconodule_reading) — which line does the commandment itself draw?',
    'Exegetical and doctrinal adjudication; structurally, observe which victim sets each reading generates — the strict reading criminalizes all image production, this reading criminalizes statuary and regulates flat work, the iconodule reading criminalizes only worship acts directed at images.',
    'Under iconoclast_reading, epsilon rises and the victim set expands to all image-makers and image-owning laity; under iconodule_reading, the licensing and gatekeeping function dissolves and epsilon falls toward coordination cost, shifting the classification away from snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_scope_line_disagreement, conceptual, 'Sibling readings change the victim set and epsilon by moving the prohibition''s scope line.').

omega_variable(
    regulation_function_ambiguity,
    'Does the detailed regulation of permissible flat images function primarily to prevent idolatrous abuse, or to manufacture compliance dependency that sustains the authority''s gatekeeping position?',
    'Compare documented image-abuse rates across communities holding identical doctrine but living under different enforcement intensities; if abuse does not track enforcement intensity, the regulation''s operative function is authority maintenance rather than abuse prevention.',
    'Gatekeeping-dominant confirms the snare classification and the named gain_flow seat; abuse-prevention-dominant would support reclassification toward tangled_rope with the authority as a coordinating administrator rather than a captor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulation_function_ambiguity, empirical, 'Whether the monitoring apparatus prevents abuse or manufactures dependency.').

omega_variable(
    suppression_internalization_mix,
    'Is lay compliance with the image code maintained by enforcement (confiscation, fines, inspection) or by formed conscience — believers catechized to experience images, especially carved ones, as spiritually dangerous?',
    'Post-deregulation trajectory: if devotional statuary demand revives quickly where enforcement lapses, suppression was structural; if communities continue avoiding images absent enforcement, suppression was substantially internalized.',
    'Internalized suppression means effective suppression exceeds the structural measure and would persist after regime collapse; structural suppression predicts rapid rearrangement on enforcement decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mix, empirical, 'Structural versus internalized component of measured suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__moderate_iconoclast_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deca_tr_t0, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(deca_tr_t8, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(deca_tr_t16, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(deca_tr_t24, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(deca_tr_t32, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(deca_tr_t40, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(deca_tr_t48, decalogue_image_prohibition__moderate_iconoclast_reading, theater_ratio, 48, 0.3).

% Extraction over time
narrative_ontology:measurement(deca_be_t0, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deca_be_t8, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(deca_be_t16, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(deca_be_t24, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(deca_be_t32, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 32, 0.7).
narrative_ontology:measurement(deca_be_t40, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(deca_be_t48, decalogue_image_prohibition__moderate_iconoclast_reading, base_extractiveness, 48, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deca_su_t0, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(deca_su_t8, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(deca_su_t16, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(deca_su_t24, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(deca_su_t32, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(deca_su_t40, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(deca_su_t48, decalogue_image_prohibition__moderate_iconoclast_reading, suppression_requirement, 48, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__moderate_iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconoclast_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__moderate_iconoclast_reading, iconodule_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Decalogue's image prohibition' decomposes into three structurally distinct constraints corresponding to the three readings of the kernel. Each reading fixes its own epsilon, victim set, and type: the strict reading criminalizes all religious imagery (highest epsilon, largest victim set); this moderate reading criminalizes statuary and regulates flat work (high epsilon, gatekeeping-mediated); the iconodule reading criminalizes only acts of image-worship (epsilon near coordination cost). The strict reading functions upstream of this one — its premises about material danger are cited as warrant for the graduated code — and this reading's existence pressures the iconodule reading by occupying the regulatory space a worship-targeted reading would vacate. This file authors only the moderate reading; the siblings are separate stories linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(decalogue_image_prohibition__moderate_iconoclast_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
