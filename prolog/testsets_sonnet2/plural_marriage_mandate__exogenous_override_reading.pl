% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally Coerced Abandonment of Divine Mandate
 *   domain: religious institutional history / political theology / commitment systems
 *
 * SUMMARY:
 *   This story instantiates the exogenous-override reading of the plural
 *   marriage mandate kernel: the claim that the 1890 Manifesto was not a
 *   legitimate prophetic reinterpretation but a forced abandonment of a
 *   divinely commanded practice, extracted through escalating federal
 *   coercion (the Morrill, Edmunds, and Edmunds-Tucker Acts;
 *   disincorporation; property escheat; mass imprisonment of cohabiting
 *   husbands). On this reading, the church's own revelation narrative
 *   functions as legitimating cover for capitulation to a materially superior
 *   coercive power, and the persons who actually bear the cost — practicing
 *   polygamist households — are the victims of a snare whose coordination
 *   story (orderly, revelation-driven institutional evolution) is authored
 *   by, and serves, the parties who suffered none of its costs. Two sibling
 *   constraints read the same 1890 Manifesto text differently: the
 *   endogenous_reinterpretation_reading treats the revelation claim as
 *   theologically genuine (a Mountain/Rope-leaning reading with a very
 *   different beneficiary structure), and the
 *   institutional_pragmatism_reading treats the doctrinal frame as an
 *   instrumentally adopted survival narrative layered over strategic
 *   capitulation (a Tangled Rope-leaning reading emphasizing institutional
 *   self-preservation over pure coercion). Per the ε-invariance principle,
 *   each reading is authored here as its own constraint with its own stable
 *   ε; this file authors only the exogenous-override claim.
 *
 * KEY AGENTS:
 *   - federal_government: primary beneficiary/agenda_setter (institutional/analytical) — achieves territorial conformity and dissolution of a rival order at negligible cost to itself
 *   - practicing_polygamist_families and plural_wives: primary victims (powerless/trapped) — bear imprisonment, property loss, and covenant abandonment with no structural exit
 *   - church_hierarchy: coerced transmission channel and partial beneficiary (institutional/constrained) — issues the Manifesto under duress but survives institutionally and retains authority
 *   - later_historians_and_church_apologists: excluded analytical voice — hold the sibling readings this story deliberately does not adjudicate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.81).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "1890 Manifesto as Federally Coerced Abandonment of Divine Mandate").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious institutional history / political theology / commitment systems").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '12c37886-b1fd-4fc8-b445-34309d6dcba4').
narrative_ontology:cs_kernel_codification('12c37886-b1fd-4fc8-b445-34309d6dcba4', formalized).
narrative_ontology:cs_authority_grounding('12c37886-b1fd-4fc8-b445-34309d6dcba4', lineage).
narrative_ontology:cs_interpretation_layer_present('12c37886-b1fd-4fc8-b445-34309d6dcba4').
narrative_ontology:cs_reading_relation('12c37886-b1fd-4fc8-b445-34309d6dcba4', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('12c37886-b1fd-4fc8-b445-34309d6dcba4', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('12c37886-b1fd-4fc8-b445-34309d6dcba4', foundational, temporal_authority_cannot_licitly_override_divine_command).
narrative_ontology:cs_axiom_status(temporal_authority_cannot_licitly_override_divine_command, holdable).
narrative_ontology:cs_axiom_grounding('12c37886-b1fd-4fc8-b445-34309d6dcba4', temporal_authority_cannot_licitly_override_divine_command, deontological).
narrative_ontology:cs_axiom('12c37886-b1fd-4fc8-b445-34309d6dcba4', foundational, coercion_extracted_compliance_is_not_valid_revelation).
narrative_ontology:cs_axiom_status(coercion_extracted_compliance_is_not_valid_revelation, holdable).
narrative_ontology:cs_axiom_grounding('12c37886-b1fd-4fc8-b445-34309d6dcba4', coercion_extracted_compliance_is_not_valid_revelation, empirically_contingent).
narrative_ontology:cs_reference_frame('12c37886-b1fd-4fc8-b445-34309d6dcba4', continuous_revelation_doctrinal_supremacy).
narrative_ontology:cs_drift_state('12c37886-b1fd-4fc8-b445-34309d6dcba4', post_edmunds_tucker_enforcement_peak, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('12c37886-b1fd-4fc8-b445-34309d6dcba4', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalitions).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, plural_wives).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, children_of_plural_marriages).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_membership_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, church_hierarchy).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_supremacy_over_territorial_religious_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces the Edmunds and Edmunds-Tucker Acts: disincorporates the church, seizes church property above a statutory cap, disenfranchises polygamists, and imprisons practitioners. Escalates pressure until the Manifesto is issued, then normalizes relations and admits the territory to statehood. Bears essentially no cost from the arrangement and collects the outcome it sought — conformity to national marriage law and dissolution of a rival theocratic-economic order.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% National reform, women's, and Protestant religious organizations that campaigned for federal intervention, framing plural marriage as a moral and civilizational threat. They lobby Congress, supply testimony and public pressure for the escalating legislation, and claim vindication when the Manifesto is issued, though they bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, anti_polygamy_reform_coalitions, beneficiary,
    organized, generational, mobile, national).

% Husbands face arrest, fines, and imprisonment under cohabitation statutes; households face property seizure and disenfranchisement. Many go into hiding ('the Underground') or flee to Mexico or Canada. After 1890 they are told the practice they entered into as a sacred, eternal covenant is suspended, but pre-existing plural marriages are not immediately dissolved, leaving them in continued legal jeopardy with no clean exit from either the law or the covenant they already made.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamist_families, payer,
    powerless, biographical, trapped, regional).

% Bear the direct social, legal, and economic consequences of the crackdown: loss of legal standing as wives, loss of inheritance and property claims, forced testimony against husbands in some prosecutions, and after the Manifesto, ambiguous status as women in relationships the institution can no longer defend but has not disowned them for entering. Exit from the marriage carries its own severe social and economic cost; exit from the territory is rarely realistic.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, plural_wives, payer,
    powerless, biographical, trapped, regional).

% Bear reputational and legal stigma as offspring of relationships the state criminalizes and the church subsequently disavows publicly while quietly continuing to solemnize some new plural unions for another decade. Have no voice in either the practice or its abandonment and no capacity to exit their family situation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, children_of_plural_marriages, payer,
    powerless, biographical, trapped, regional).

% Faces institutional dissolution, seized temples, and the practical destruction of church governance if resistance continues. Issues the Manifesto, publicly frames it as revelation, and thereby preserves ecclesiastical continuity, control over the remaining assets, and the path to statehood and political normalization for the territory it governs. On this reading, the institution is simultaneously the transmission channel of the coercion and a partial beneficiary of the settlement it produces, since it survives and its leadership retains authority.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_hierarchy, beneficiary).

% The broader membership, most of whom were never polygamous, absorb the doctrinal whiplash and the reputational cost of decades of federal antagonism, and are asked to accept a revelation narrative for what, on this reading, is externally compelled capitulation. They bear the ongoing cost of reconciling continued belief in continuous revelation with a reversal driven by the visible chronology of legislative and judicial escalation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_membership_at_large, payer,
    moderate, generational, constrained, regional).

% Administer the Edmunds-Tucker Act's enforcement machinery: prosecute cohabitation cases, uphold escheat of church property in Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States (1890), and require an anti-polygamy oath as a precondition for restored corporate status and statehood, directly precipitating the Manifesto's timing.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_courts_and_prosecutors, agenda_setter,
    institutional, biographical, analytical, national).

% Would contest this reading's framing directly, arguing the Manifesto text and subsequent church discourse describe genuine prophetic revelation rather than mere capitulation; they are not absent from the historical record but their counter-framing is excluded from THIS reading by construction, since this constraint is authored strictly as the exogenous-coercion account (see sibling readings for their position).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, later_historians_and_church_apologists, excluded,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine on this reading: there is no mutual problem being solved between the federal government and the church membership. What looks like coordination (an orderly, negotiated cessation) is actually the terminal stage of a one-sided coercive campaign in which the coerced party's compliance is relabeled as voluntary religious development.
% TRANSFER_FUNCTION: Moves practical religious authority, marital legal status, and control over communal property from the church and its plural-marriage households to the federal government and the national moral-reform coalition that lobbied for intervention; moves severe personal and legal risk onto individual polygamist men, their plural wives, and their children.
% ABSENT_VOICES: Practicing polygamist wives and children are almost entirely absent from the legislative record and from the Manifesto's own text, which speaks in the voice of church leadership addressing the nation, not in the voice of those whose marriages and households were being dissolved by decree. Their objections, where recorded at all, survive mainly in private diaries and later oral history, not in the institutional record that produced the settlement.
% DISAPPEARANCE_RATIONALE: If the federal coercive apparatus (disincorporation, property seizure, disenfranchisement, imprisonment) had not existed, there is no structural reason internal to the church's own theological trajectory that requires the 1890 timing or form of the Manifesto; the practice's suspension tracks the litigation and property-seizure calendar too precisely to be independent of it. Remove the coercion and, on this reading, the institutional and marital arrangements reorganize around continued practice.
% FOUNDING_PROBLEM: The federal government sought to eliminate plural marriage as a rival social-legal order it viewed as incompatible with republican governance, national marriage law, and Protestant-influenced moral consensus, and to break the church's territorial economic and political dominance in Utah as a precondition for statehood.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the church's own institutional apparatus — including legal historians examining the Edmunds-Tucker Act's escheat provisions and the timing of the Late Corporation decision relative to the Manifesto's issuance — corroborate that the coercive federal objective (elimination of plural marriage as a legal-political threat) was substantially achieved and is no longer a live governmental concern; the church's own official historical narrative, by contrast, treats the founding problem as resolved through revelation rather than through the coercive record, which is precisely the interpretive dispute this reading exists to name.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply from 1862 (first federal anti-polygamy statute, largely unenforced, ε≈0.35) through 1887 (Edmunds-Tucker disincorporation and escheat, ε≈0.78) to peak at the 1890 Manifesto itself (ε=0.81), reflecting the maximal coercive pressure at the moment of capitulation on this reading. Suppression tracks the same curve but peaks slightly earlier (1887, 0.90) at the height of prosecutions and asset seizure, then recedes as enforcement relaxes once compliance is secured — suppression is authored as the raw, unscaled coercive force applied, distinct from extractiveness. Theater ratio rises steadily and continues rising past 1890 (reaching 0.60 by 1904) because, on this reading, the revelation narrative itself is the theatrical layer: it grows more elaborate and more institutionally load-bearing over time even as the underlying coercive apparatus recedes, precisely because the narrative must do more legitimating work as the coercive facts fade from public memory.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat this looks like successful, low-cost policy enforcement culminating in orderly voluntary compliance — a rope from that vantage. From the practicing polygamist households' seat, the same sequence of events is a snare: a coordination story (church governance, doctrinal continuity, national integration) laid over an extraction of covenant, property, and liberty. The engine computes these as different seat-level types from the same structural data; this story authors the structural data from the exogenous-override reading's own lights, per the fixed ε-referent rule — ε describes the standing coerced-abandonment arrangement as this reading sees it, not the abolitionist or reinterpretation alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and reform coalitions are declared beneficiaries with mobile/analytical exit — they collect the outcome (territorial conformity, moral vindication) without bearing enforcement costs, so directionality derives toward the beneficiary end. Practicing polygamist families, plural wives, and their children are declared victims with trapped exit options — no meaningful geographic or legal escape existed during the enforcement peak — so directionality derives toward the full-target end, amplifying effective extraction for exactly the agents least able to bear it. Church hierarchy is deliberately given a dual role (agenda_setter/beneficiary) with constrained rather than trapped exit: on this reading it is coerced, but it is coerced into a settlement that also secures its own institutional survival, which the schema's dual-role mechanism is built to represent without collapsing the asymmetry between the church's constrained exit and its members' trapped exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal elimination of a rival territorial-legal order) is authored as dead — achieved by 1896 statehood and essentially complete by 1904 — while the church's own disappearance-verdict-adjacent narrative (continuous revelation) persists indefinitely. This mismatch (status=dead, but the institutional narrative behaves as though the underlying moral/theological problem is still live) is exactly the founding_problem consumption rule's target: it is not consumed here as a claim about what the Manifesto WAS, but the status/corroboration pairing flags the capture-pattern risk that a reading-external observer would want to check against the computed piton/theater path for the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_coercion_causality,
    'Was the 1890 Manifesto a genuine independent theological development that happened to coincide with federal pressure, or was it causally produced by that pressure such that absent the coercion it would not have occurred when or as it did?',
    'Close comparison of the timing, language, and internal church correspondence around the Manifesto against the litigation and property-seizure calendar (particularly the Late Corporation decision and Edmunds-Tucker escheat deadlines); examination of whether church leaders privately described the decision as coerced versus revealed in contemporaneous, non-public records.',
    'If the causal link to coercion is tight and internally acknowledged, this exogenous-override reading is strongly supported and the constraint is properly a snare. If internal records show independent theological deliberation substantially predating or decoupled from the legal pressure, the endogenous_reinterpretation_reading gains support and this reading''s ε would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_coercion_causality, empirical, 'Whether the Manifesto''s timing and content were causally driven by coercion or independently theologically motivated.').

omega_variable(
    kernel_reading_incommensurability,
    'Are the three kernel readings (exogenous override, endogenous reinterpretation, institutional pragmatism) genuinely mutually exclusive claims about the same historical event, or do they describe compatible partial truths that a single richer framework could hold simultaneously?',
    'This is the committer-structure question the kernel decomposition exists to isolate: examine whether any single party (a specific historian, a specific church leader''s private account) actually holds more than one of these readings at once without contradiction, versus whether the readings are held by structurally distinct communities (state historians vs. church apologists vs. institutional-survival theorists) who do not cross over.',
    'If a single coherent framework could hold both coercion-causality AND genuine-if-reluctant revelation (i.e., God permitting or ratifying a coerced outcome), the forecloses relation to the endogenous reading would be wrong and coexists_with is the correct edge. This omega documents that the reading_relations below are authored as coexists_with rather than forecloses precisely because the readings are held by different communities without logical contradiction within a single framework being clearly established.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s three readings are logically incompatible or coexisting partial framings held by different communities.').

omega_variable(
    church_hierarchy_dual_role_ambiguity,
    'Is the church hierarchy best modeled as a coerced victim-transmitter of federal pressure, a partial beneficiary that used the coercion to consolidate internal authority and shed a costly practice, or both simultaneously?',
    'Examine whether church leadership''s post-1890 institutional consolidation (property retention, statehood negotiation leverage, internal authority over remaining practitioners) exceeded what mere survival would require, versus whether all such gains are fully explained by minimal coercion-response.',
    'If the hierarchy''s institutional gains substantially exceed bare survival, the dual agenda_setter/beneficiary role authored here is well-supported and the church sits closer to the institutional_pragmatism_reading''s structure even within this exogenous-override file; if gains are minimal, the hierarchy should be modeled as a nearly pure coerced-transmitter with directionality closer to the victim end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_hierarchy_dual_role_ambiguity, empirical, 'Whether church leadership meaningfully benefited from the coerced settlement beyond bare institutional survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1862, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(plur_tr_t1874, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1874, 0.15).
narrative_ontology:measurement(plur_tr_t1882, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1882, 0.2).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1887, 0.28).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1890, 0.42).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1896, 0.55).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 1904, 0.6).

% Extraction over time
narrative_ontology:measurement(plur_be_t1862, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1862, 0.35).
narrative_ontology:measurement(plur_be_t1874, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1874, 0.48).
narrative_ontology:measurement(plur_be_t1882, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1882, 0.62).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1887, 0.78).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1890, 0.81).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1896, 0.7).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 1904, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1862, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1862, 0.25).
narrative_ontology:measurement(plur_su_t1874, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1874, 0.45).
narrative_ontology:measurement(plur_su_t1882, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1882, 0.68).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1887, 0.9).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1890, 0.88).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1896, 0.55).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 1904, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__exogenous_override_reading, institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the plural_marriage_mandate kernel (the 1890 Manifesto and its aftermath). All three readings are linked via affects_constraints. The endogenous_reinterpretation_reading authors near-zero extraction (genuine revelation, no victims in the coercion sense); the institutional_pragmatism_reading authors moderate-high extraction concentrated on institutional self-preservation with the doctrinal claim as legitimating cover (tangled_rope); this exogenous_override_reading authors the highest extraction and the clearest victim set (practicing polygamist households) as a snare. The three files share the historical kernel but diverge on ε, type, and beneficiary/victim structure by design, per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
