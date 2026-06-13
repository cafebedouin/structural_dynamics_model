% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Durable Divine Mandate for Categorical Separation (Durable-Separation Reading)
 *   domain: religious/ethical/committal
 *
 * SUMMARY:
 *   The herem command (Deuteronomy 7, and conquest narratives) directs
 *   ancient Israel to destroy or exclude designated outsider populations to
 *   preserve covenant purity. The durable-separation reading interprets herem
 *   as encoding a timeless divine mandate: categorical separation from all
 *   designated outsiders, with intermarriage as covenant violation and
 *   outsider status as permanent unless conversion erases prior identity.
 *   This reading benefits preservationist institutions that use herem's
 *   authority to enforce marriage rules, doctrinal conformity, and boundary
 *   maintenance. It extracts autonomy from intermarriage practitioners
 *   (identity-locked choice suppression) and legitimates categorical
 *   exclusion of ethnic outsiders. The reading is contested: allegorical and
 *   contextual readings reject its core claims. The constraint story models
 *   THIS reading as a clean ε-invariant constraint, not a description of 'the
 *   herem command' generically. The claim is tangled_rope: the coordination
 *   function (identity preservation against assimilation) is genuine; the
 *   extraction (suppression of marriage autonomy, victim expansion to all
 *   outsiders) is asymmetric and actively enforced. The authority structure
 *   grounds itself in lineage (transmitted interpretation of divine text) and
 *   uses extraction (institutional benefit from the reading's permanence) to
 *   maintain its codification.
 *
 * KEY AGENTS:
 *   - covenant_community_preservationists: institutional beneficiary, identity-locked agenda-setter; interprets and enforces boundary-permanent reading
 *   - intermarriage_practitioners: moderate-power payer, identity-locked exit; primary target of marriage-choice suppression
 *   - ethnic_outsiders_designated_herem: powerless payer, trapped exit; ascriptively designated as contamination threat; victims of legitimated exclusion/violence
 *   - theological_dissenters: moderate-power payer/beneficiary, constrained exit; bear doctrinal coercion but benefit from community membership
 *   - conversion_candidates: powerless payer/beneficiary, constrained exit; benefit from community inclusion; pay through prior-identity extraction
 *   - hermeneutical_authorities: institutional observer, analytical exit; analyze readings but do not set policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.91).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Durable Divine Mandate for Categorical Separation (Durable-Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious/ethical/committal").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, 'b19a60de-a377-4dc2-bf03-fd31c7963102').
narrative_ontology:cs_kernel_codification('b19a60de-a377-4dc2-bf03-fd31c7963102', fixed_text).
narrative_ontology:cs_authority_grounding('b19a60de-a377-4dc2-bf03-fd31c7963102', extraction).
narrative_ontology:cs_interpretation_layer_present('b19a60de-a377-4dc2-bf03-fd31c7963102').
narrative_ontology:cs_reading_relation('b19a60de-a377-4dc2-bf03-fd31c7963102', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_reading_relation('b19a60de-a377-4dc2-bf03-fd31c7963102', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('b19a60de-a377-4dc2-bf03-fd31c7963102', foundational, divine_mandate_permanence).
narrative_ontology:cs_axiom_status(divine_mandate_permanence, holdable).
narrative_ontology:cs_axiom_grounding('b19a60de-a377-4dc2-bf03-fd31c7963102', divine_mandate_permanence, deontological).
narrative_ontology:cs_axiom('b19a60de-a377-4dc2-bf03-fd31c7963102', foundational, categorical_boundary_theology).
narrative_ontology:cs_axiom_status(categorical_boundary_theology, holdable).
narrative_ontology:cs_axiom_grounding('b19a60de-a377-4dc2-bf03-fd31c7963102', categorical_boundary_theology, theological).
narrative_ontology:cs_reference_frame('b19a60de-a377-4dc2-bf03-fd31c7963102', covenant_purity_framework).
narrative_ontology:cs_drift_state('b19a60de-a377-4dc2-bf03-fd31c7963102', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b19a60de-a377-4dc2-bf03-fd31c7963102', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_preservationists).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_practitioners).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, ethnic_outsiders_designated_herem).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, theological_dissenters_from_durable_reading).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, theological_dissenters_from_durable_reading).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, conversion_candidates).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, conversion_candidates).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_mandate_permanence).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_boundary_theology).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_community_purity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce herem as a standing divine command binding on the covenant community across all historical periods. They administer the boundary-maintenance interpretation: the command to separate from outsiders is permanent, justified as necessary to preserve covenant identity and fidelity. They benefit from the interpretation's legitimacy — it justifies institutional control over membership, marriage rules, and doctrinal conformity. Their exit from this reading would dissolve their authority frame.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_preservationists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Seek or maintain spousal/familial relationships across covenant boundaries (marrying non-believers, outsiders, ethnic groups designated as herem-subject). The durable-separation reading treats this as covenant violation, triggering institutional sanctions (exclusion, shunning, family rupture). They bear the extraction directly: autonomy over marriage choice is suppressed; continued participation in the covenant community requires severing the relationship or converting the partner (high identity-locked cost). Exit from the constraint means accepting community expulsion.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_practitioners, payer,
    moderate, biographical, identity_locked, global).

% Are categorically designated as legitimate targets of separation, exclusion, and (in historical instantiations of the reading) violence. The durable-separation reading encodes them as perpetual contamination threats to covenant purity, regardless of individual conduct or conversion. They cannot exit the designation — it is ascriptive and permanent under this reading. Their options are assimilation (converting, severing all identity roots) or accepting exclusion. The constraint legitimates institutional discrimination and historical violence against them as obedience to divine command.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, ethnic_outsiders_designated_herem, payer,
    powerless, generational, trapped, global).

% Hold alternative readings (allegorical, contextual, supersessionist) that diminish or reject herem's binding force on contemporary life. They bear institutional pressure from preservationist authority to conform their interpretation. They may benefit from some aspects of covenant community membership but pay through doctrinal coercion and reduced standing if they dissent openly. Exit from the dissent suppresses their genuine hermeneutic commitment; full exit from the community severs identity and kinship networks.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, theological_dissenters_from_durable_reading, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, theological_dissenters_from_durable_reading, beneficiary).

% Outsiders seeking to join the covenant community through conversion. The durable-separation reading permits their admission only through complete identity transfer (full adoption of covenant norms, severance from original community and kinship, acceptance of boundary rules). They benefit from covenant-community membership and the legitimacy it offers; they pay through the extraction of prior identity and autonomous choice over which norms to adopt. The conversion process operationalizes the boundary extraction.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, conversion_candidates, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, conversion_candidates, payer).

% In historical conquest narratives (Canaanite populations, Amalekites, Midianites), were subject to herem implementation as extermination or enslavement. The durable-separation reading legitimates these historical actions as obedience to divine command and projects the same legitimacy onto contemporary categorical separation. They would argue (were their voices included) that the reading weaponizes ancient command to justify ongoing exclusion and historical injustice; they are structurally excluded from the theological conversation that justifies their treatment.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, historical_conquest_subjects, excluded,
    powerless, biographical, trapped, regional).

% Scholars, theologians, and religious authorities who analyze how herem is read and what constraints it generates. They examine which readings activate which extraction mechanisms, how the durable-separation reading differs from alternatives, and what structural consequences follow from each. They do not set the reading but can influence institutional adoption or rejection of particular interpretations through scholarly authority.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, hermeneutical_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_preservationists).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves covenant-community identity across generations by encoding clear categorical boundaries between members and outsiders. Solves the problem: 'How does a bounded religious group maintain distinctive identity, practice, and belief in the face of cultural assimilation and intermarriage?'
% TRANSFER_FUNCTION: Extracts autonomy over marriage choice, affiliation, and doctrinal interpretation from intermarriage practitioners and theological dissenters, and transfers categorical exclusion/violence legitimacy onto designated ethnic outsiders. Moves the cost of boundary maintenance from the preservationist institution onto the bodies and choices of those who transgress or are born outside the boundary.
% ABSENT_VOICES: Ethnic outsiders designated herem-subject have no institutional voice in the theological conversation that legitimates their treatment. Historical conquest subjects (Canaanites, Amalekites) are entirely excluded from the conversation — their own objections to the herem command are structured out of the canonical text. Women within the covenant community who might oppose the gendered enforcement of intermarriage rules have limited formal voice. Intermarriage practitioners and conversion candidates are present but under coercive power imbalance.
% DISAPPEARANCE_RATIONALE: If the durable-separation reading's legitimacy collapsed overnight, covenant communities relying on it for institutional identity would face immediate reorganization: marriage rules would lose divine sanction; boundary enforcement would require explicit consensual justification rather than divine command obedience; excluded populations would cease to be treated as perpetual contamination threats; the theological warrant for historical violence would be withdrawn (though historical injustices would remain unrepaired). Institutions built on the reading's authority would face legitimacy crisis.
% FOUNDING_PROBLEM: Ancient Israel required categorical identity preservation against cultural assimilation during settlement and consolidation. The herem command (Deuteronomy 7, and parallel conquest narratives) provided a binding mechanism: separation from outsiders encoded as divine mandate, making boundary violations into covenant breach rather than mere cultural preference.
% FOUNDING_PROBLEM_CORROBORATION: Preservationists attest the problem remains live: contemporary covenant communities still face assimilation pressure and identity fragmentation. Contextual-supersession and allegorical readings contest whether the ancient founding problem persists with the same force and whether herem remains the appropriate response. Scholars of biblical history and comparative religion (outside the benefiting parties) attest that ancient settlement-period identity preservation was a genuine problem; they dispute whether the durable-separation reading's framing of herem as timeless response is historically or theologically warranted. No voice from the designated outsider populations corroborates the framing — by construction, their corroboration is not sought.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the reading suppresses marriage autonomy entirely (not merely discouraging intermarriage, but treating it as covenant breach requiring expulsion or conversion) and expands the victim set to ALL non-covenant outsiders (not just historical herem targets). The extraction grows over the 2000-year interval as institutional codification hardens: early period shows uncertainty about whether the reading applies to contemporary populations; by late period, the reading is crystallized as standing mandate. Suppression is very high (0.91) because the constraint operationalizes through institutional enforcement (shunning, expulsion, family rupture) backed by divine-command legitimacy — the external enforcement is reinforced by belief internalization. Theater is moderate (0.28) and rising: the coordination function (identity preservation) is genuine, but enforcement increasingly emphasizes boundary-exclusion over actual assimilation prevention. Accessibility of alternatives is high (0.88): once the reading is understood, intermarriage practitioners face expulsion or conversion; no intermediate exit preserves community membership and marriage choice together. Resistance is substantial (0.67): alternative readings persist; secular law in many jurisdictions protects intermarriage; modern communication enables boundary-crossing. The measurement series show extraction and suppression accumulating over time, with theater rising as enforcement emphasis shifts from coordination to pure exclusion.
 *
 * PERSPECTIVAL GAP:
 *   Preservationists experience this constraint as coordination: a necessary framework for preserving identity against assimilation. They see herem as divinely mandated, non-negotiable, and beneficial to the community. Intermarriage practitioners experience it as pure extraction: their autonomy is suppressed by institutional power, and exit requires severing family and identity. Designated outsiders experience categorical violence legitimation: they are treated as perpetual contamination threats regardless of individual conduct. The engine computes per-seat classifications from the power/exit/beneficiary structure; the preservationist seat (institutional power, identity-locked commitment to the reading, benefits from enforcing it) computes differently from the intermarriage-practitioner seat (moderate power, identity-locked but to their marriage choice not the reading, no benefit). This divergence is exactly the extraction the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Preservationists have low directionality (near 0.0): they benefit from the constraint, hold institutional power to enforce it, and their exit from it dissolves their authority frame (high identity-lock to the reading itself, not to the community role). Intermarriage practitioners have high directionality (near 1.0): they bear the extraction directly (marriage suppression), face identity-locked coercion (exiting the constraint means community expulsion and family rupture), and have no institutional power to negotiate the rule. Designated outsiders have extremely high directionality: they are powerless, trapped by ascriptive designation, and bear both institutional exclusion and historical violence legitimation. Theological dissenters sit between: they benefit from some aspects of community membership but pay through doctrinal coercion; their exit from the dissent suppresses their genuine interpretation; full exit from the community severs identity networks. The directionality structure follows from the beneficiary/victim declarations and exit asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The durable-separation reading avoids mandatrophy classification as false-positive because the founding problem (preserving identity against assimilation) remains contested as live: contemporary preservationist institutions attest ongoing assimilation pressure; secular law, intermarriage rates, and diaspora dynamics support their concern. The reading's mandate is not dead. However, the extractiveness trajectory shows accumulating theater: enforcement emphasis shifts from addressing actual assimilation mechanisms to pure boundary-exclusion. The conversion process, for example, starts as path to inclusion and hardens into identity-erasure requirement. This accumulation is flagged by the theater metric and measurement series, not by mandatrophy. The constraint remains extractive but the mandated function (assimilation prevention) becomes an increasingly smaller share of what the enforcement actually does.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_permanence_ambiguity,
    'Is herem a timeless divine command, or a historically-bounded directive for ancient Israel''s settlement period that later tradition reinterpreted?',
    'Textual analysis of herem''s canonical context (Deuteronomy 7 vs. Leviticus 19:33-34''s protections for resident aliens; post-Pentateuchal prophetic reversals like Jonah and Ruth); historical-critical evidence of when the boundary-enforcement reading was crystallized vs. when universalist readings emerged.',
    'If timeless: the durable-separation reading holds; the constraint''s extractiveness is justified as obedience to permanent divine will. If historically-bounded: the contextual-supersession reading forecloses the durable-separation reading''s core claim — the reading becomes indefensible within frameworks accepting historical-critical analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_permanence_ambiguity, empirical, 'Whether herem''s binding force is timeless or historically-bounded.').

omega_variable(
    categorical_outsider_status_ambiguity,
    'Does herem apply to any non-covenant outsider permanently, or only to specific populations in specific contexts (Canaanites at conquest; Amalekites as historical enemies)?',
    'Close reading of herem texts and their application scope; historical reconstruction of how the reading was applied across different eras and geographies; examination of whether conversion/assimilation actually removes the herem designation or merely suspends it.',
    'If permanently categorical: the victim set is expansive (all non-covenant outsiders) and the extraction operates via ascription. If context-specific: the reading is less extractive on identity-outsiders who have no historical relationship to the original herem targets; conversion becomes a genuine exit route rather than identity-erasure masquerading as inclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_outsider_status_ambiguity, conceptual, 'Whether herem''s victim set is permanent or contextually bounded.').

omega_variable(
    violence_legitimation_via_obedience,
    'Does the durable-separation reading''s appeal to divine command obedience legitimize historical violence (conquest, extermination, enslavement), or does it merely justify institutional separation and boundary enforcement?',
    'Textual examination of how the reading historically justified genocidal conquest and contemporary institutional violence; comparison with how the allegorical and contextual readings explicitly reject violence legitimation; tracking whether institutional enforcement of the reading operationalizes physical harm or ''only'' social exclusion.',
    'If violence is legitimated: the reading''s suppression metric is higher, and the extraction includes existential threat on the designated outsider victim set. If limited to institutional separation: the extraction is mainly on marriage autonomy and doctrinal conformity, lower on the violence dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_legitimation_via_obedience, conceptual, 'Whether obedience to the durable-separation reading extends to violence legitimation or is limited to institutional boundary enforcement.').

omega_variable(
    internalized_suppression_mechanism,
    'Is the suppression of intermarriage choice structurally imposed (legal/social barriers, institutional sanctions) or substantially internalized (covenant members genuinely believe the separation is divinely mandated and morally required)?',
    'Post-exit testimony from those who left the community: do intermarriage practitioners retain suppressive beliefs and self-surveillance after exit, or does the suppression dissipate? Comparison of belief-adoption patterns in voluntary-adoption members vs. born-into members; examination of whether doubters who remain are suppressed by external enforcement or internal conviction.',
    'If internalized: the effective suppression operates across the agent''s full life even after exit; the constraint is more extractive than the structural measure suggests. If structural: exit removes the suppression; the external enforcement is the extraction mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_mechanism, empirical, 'Structural vs. internalized suppression of boundary-crossing choice.').

omega_variable(
    kernel_reading_contest_asymmetry,
    'This constraint is ONE reading of the herem kernel. The allegorical_displacement_reading reinterprets ''nations'' as typological placeholders for spiritual enemies (sin, temptation) not ethnic groups; the contextual_supersession_reading treats herem as historically-bounded and morally superseded. Does the durable-separation reading foreclose these siblings or coexist with them?',
    'Examine whether the durable-separation reading''s core axioms (divine mandate permanence, categorical boundary theology) logically rule out the siblings'' core premises, or whether different theological communities genuinely hold incompatible readings without logical foreclosure within each community''s framework.',
    'If forecloses: the sibling readings are incoherent under this reading''s epistemic frame, and institutional adoption of this reading eliminates the others. If coexists: competing readings persist across different communities/traditions even when holding the same canonical texts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_asymmetry, conceptual, 'Logical status of the three herem readings in relation to each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(here_tr_t0, projected).
narrative_ontology:measurement(here_tr_t250, herem_command_dt7__durable_separation_reading, theater_ratio, 250, 0.14).
narrative_ontology:measurement_basis(here_tr_t250, observed).
narrative_ontology:measurement(here_tr_t500, herem_command_dt7__durable_separation_reading, theater_ratio, 500, 0.17).
narrative_ontology:measurement_basis(here_tr_t500, observed).
narrative_ontology:measurement(here_tr_t1000, herem_command_dt7__durable_separation_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement_basis(here_tr_t1000, observed).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__durable_separation_reading, theater_ratio, 1500, 0.26).
narrative_ontology:measurement_basis(here_tr_t1500, observed).
narrative_ontology:measurement(here_tr_t2000, herem_command_dt7__durable_separation_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(here_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(here_be_t0, projected).
narrative_ontology:measurement(here_be_t250, herem_command_dt7__durable_separation_reading, base_extractiveness, 250, 0.73).
narrative_ontology:measurement_basis(here_be_t250, observed).
narrative_ontology:measurement(here_be_t500, herem_command_dt7__durable_separation_reading, base_extractiveness, 500, 0.75).
narrative_ontology:measurement_basis(here_be_t500, observed).
narrative_ontology:measurement(here_be_t1000, herem_command_dt7__durable_separation_reading, base_extractiveness, 1000, 0.79).
narrative_ontology:measurement_basis(here_be_t1000, observed).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__durable_separation_reading, base_extractiveness, 1500, 0.81).
narrative_ontology:measurement_basis(here_be_t1500, observed).
narrative_ontology:measurement(here_be_t2000, herem_command_dt7__durable_separation_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(here_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement_basis(here_su_t0, projected).
narrative_ontology:measurement(here_su_t250, herem_command_dt7__durable_separation_reading, suppression_requirement, 250, 0.83).
narrative_ontology:measurement_basis(here_su_t250, observed).
narrative_ontology:measurement(here_su_t500, herem_command_dt7__durable_separation_reading, suppression_requirement, 500, 0.85).
narrative_ontology:measurement_basis(here_su_t500, observed).
narrative_ontology:measurement(here_su_t1000, herem_command_dt7__durable_separation_reading, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement_basis(here_su_t1000, observed).
narrative_ontology:measurement(here_su_t1500, herem_command_dt7__durable_separation_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement_basis(here_su_t1500, observed).
narrative_ontology:measurement(here_su_t2000, herem_command_dt7__durable_separation_reading, suppression_requirement, 2000, 0.91).
narrative_ontology:measurement_basis(here_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(herem_command_dt7__durable_separation_reading, 0.12).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel has three structurally distinct readings, each with different ε values and extraction mechanisms. The durable-separation reading (this constraint) operates with high extractiveness on marriage autonomy and categorical victim expansion. The contextual-supersession reading treats herem as historically-bounded, reducing extractiveness to identity-boundary maintenance only. The allegorical-displacement reading eliminates extractiveness on ethnic outsiders entirely by redefining the constraint's domain from ethnic groups to internal spiritual enemies. Each reading is a separate constraint file; they are linked via network.affects_constraints to document their kernel kinship. The readings do not merge; the ε-invariance principle requires separate stories because the observables differ (what counts as violating each reading is different, so ε is not invariant across readings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, powerless, 0.95).
constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
