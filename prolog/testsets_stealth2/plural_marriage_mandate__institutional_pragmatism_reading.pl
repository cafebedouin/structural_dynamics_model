% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto Arrangement — Institutional Pragmatism Reading (Strategic Capitulation Legitimated by Revelation Narrative)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'plural_marriage_mandate': the institutional pragmatism reading, under
 *   which the 1890 Manifesto is a strategic institutional adaptation in which
 *   the revelation narrative functions to legitimate survival-driven
 *   capitulation to superior federal coercive power. The standing arrangement
 *   under contest — the ε referent — is the Manifesto regime itself as this
 *   reading sees it: public doctrine unchanged (plural marriage remains a
 *   revealed principle), public practice suspended, and a managed gap between
 *   the two, including authorized secret sealings from 1890 to 1904, closed
 *   by the 1904 Second Manifesto and hardened enforcement. The primary
 *   observable under this reading is the doctrine-practice (M-set) gap.
 *   Beneficiary structure: church leadership collects institutional survival,
 *   restored corporate property, amnesty, and restored political rights; the
 *   federal government collects its policy outcome; Utah's non-Mormon
 *   population collects peace and integration. Victim structure: families who
 *   had entered plural marriage as commanded duty bear coerced reversal and
 *   household disruption; the monogamist majority sustains the institution
 *   while kept ignorant of continuations; officiants of post-Manifesto
 *   sealings ultimately absorb disciplinary enforcement. CONSTRAINT FAMILY
 *   NOTE: this is one of three linked readings of the same kernel (with
 *   exogenous_override_reading and endogenous_reinterpretation_reading); each
 *   is a separate ε-invariant constraint with its own metrics and
 *   stakeholders, linked via network.affects_constraints. CLAIM/METRIC
 *   INDEPENDENCE: claimed_type tangled_rope is asserted from structure (a
 *   genuine survival-coordination function entangled with asymmetric
 *   extraction); the metrics are authored independently as descriptive
 *   estimates — the engine computes per-seat classifications and any
 *   divergence from the claim is the datum.
 *
 * KEY AGENTS:
 *   - - lds_first_presidency: Agenda-setting beneficiary (institutional/identity_locked) — issues and administers the suspension, manages the doctrine-practice gap, collects institutional survival, restored property, and political rights
 *   - - quorum_of_the_twelve: Dual-positioned beneficiary/payer (institutional/identity_locked) — shares governance gains; members who performed or defended post-Manifesto sealings later bear enforcement costs
 *   - - federal_government: External beneficiary (institutional/arbitrage) — collects cessation of public plural marriage, Utah's integration, and demonstrated federal supremacy; retained escalation options throughout
 *   - - coerced_polygamist_families: Primary target (moderate/trapped) — bear abandonment of a commanded practice, household disruption, and stigma
 *   - - deceived_monogamist_members: Target (organized/constrained) — sustain the institution with labor and tithing while kept ignorant of 1890-1904 continuations
 *   - - post_manifesto_officiants: Target (powerful/trapped) — perform or enter post-Manifesto sealings, later absorb resignation, loss of office, and excommunication
 *   - - utah_non_mormon_residents: Incidental beneficiary (organized/mobile) — collect peace, statehood, and market integration
 *   - - fundamentalist_dissenters: Excluded voice (powerless/trapped) — insist the mandate remains binding; marginalized out of official deliberation
 *   - - religious_historians: Analytical observer (analytical/analytical) — reconstruct the gap from diaries, council minutes, and sealing records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.7).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto Arrangement — Institutional Pragmatism Reading (Strategic Capitulation Legitimated by Revelation Narrative)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd').
narrative_ontology:cs_kernel_codification('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', fixed_text).
narrative_ontology:cs_authority_grounding('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', extraction).
narrative_ontology:cs_interpretation_layer_present('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd').
narrative_ontology:cs_reading_relation('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', foundational, revelation_narrative_functions_as_legitimation).
narrative_ontology:cs_axiom_status(revelation_narrative_functions_as_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', revelation_narrative_functions_as_legitimation, empirically_contingent).
narrative_ontology:cs_axiom('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', foundational, institutional_survival_outweighs_practice_continuity).
narrative_ontology:cs_axiom_status(institutional_survival_outweighs_practice_continuity, holdable).
narrative_ontology:cs_axiom_grounding('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', institutional_survival_outweighs_practice_continuity, instrumental).
narrative_ontology:cs_reference_frame('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', survival_prioritized_institutional_governance).
narrative_ontology:cs_drift_state('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', contemporary_official_narrative, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc87f5da-ec4e-41c3-b039-a3e7c72cbbbd', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, quorum_of_the_twelve).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, federal_government).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, utah_non_mormon_residents).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_officiants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, quorum_of_the_twelve).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the office of prophet-president with counselors. Issued the 1890 declaration suspending public plural marriage, framed it as divine instruction, managed the distance between public teaching and continued private sealings, negotiated amnesty and property restoration with federal officials, and after 1904 directed disciplinary enforcement against those who continued the practice. Exit from the arrangement would require renouncing the revelatory frame that constitutes the office's own authority.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency, agenda_setter,
    institutional, generational, identity_locked, global).

% Shares in church governance, and several members performed or authorized plural sealings after 1890. Collects institutional position and continuity alongside the presidency; two members who defended continued sealings resigned under pressure in 1905 and one was later excommunicated, bearing enforcement costs the arrangement eventually imposed on its own officers.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, quorum_of_the_twelve, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, quorum_of_the_twelve, payer).

% Prosecuted polygamy through successive statutes, seized church property, disenfranchised polygamists, and blocked Utah statehood until the practice ceased. Collected the cessation of public plural marriage, Utah's integration into national political and economic life, and the precedent that federal law outranks religious command. Retained escalation options throughout, up to full corporate dissolution under the Edmunds-Tucker Act.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, beneficiary,
    institutional, generational, arbitrage, national).

% Entered plural marriage over four decades as a religious duty backed by prophetic command, accepting imprisonment risk, poverty, and ostracism. After 1890 were instructed the practice must cease; existing households faced legal limbo and congregational stigma, and husbands who sealed new wives after 1890 became subject to discipline. Leaving the faith meant losing community, family standing, and the salvific framework that structured their sacrifices.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, coerced_polygamist_families, payer,
    moderate, generational, trapped, regional).

% Comprised the large majority of membership after 1890. Were taught publicly that plural marriage had ended, sustained the institution with labor and tithing on that understanding, and were not informed that selected sealings continued until 1904. Information flowed through channels controlled by the leadership; independently verifying the gap was practically impossible from inside the community.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamist_members, payer,
    organized, biographical, constrained, continental).

% Apostles, temple presidents, and stake leaders who performed or entered plural sealings between 1890 and 1904, acting on the understanding that the declaration suspended public advocacy rather than the ordinance itself. When enforcement hardened after 1904 they faced resignation, loss of office, and excommunication; their standing inside the institution left no graceful exit.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, post_manifesto_officiants, payer,
    powerful, biographical, trapped, continental).

% Merchants, miners, and settlers outside the church who endured decades of communal conflict and economic boycotts. Collected peace, statehood, rail and capital integration, and access to markets previously closed by sectarian tension. Few were bound to the region by religious obligation, so exit remained genuinely available.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, utah_non_mormon_residents, beneficiary,
    organized, biographical, mobile, regional).

% Believers who concluded the 1890 declaration lacked revelatory authority and that the mandate remained in force. Organized informally at first, were disciplined or marginalized out of mainstream congregations, and eventually formed separate communities in the twentieth century. Their objections never entered official deliberation.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, fundamentalist_dissenters, excluded,
    powerless, generational, trapped, regional).

% Academic and independent researchers reconstructing the decision sequence from diaries, council minutes, sealing records, and federal correspondence. Hold no stake in the arrangement's persistence and publish findings the benefiting parties neither control nor canonize.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a genuine collective-action problem: an embedded religious community faced escalating federal seizure — leadership imprisonment, corporate dissolution, property forfeiture, disenfranchisement, statehood denial. Suspending public plural marriage gave every member a single survivable path that ended prosecutions, secured amnesty, and unlocked statehood, where scattered individual resistance or mass defection would have destroyed the community's capacity to function at all.
% TRANSFER_FUNCTION: Moves compliance costs — abandonment of a commanded practice, disruption of existing plural households, doctrinal dissonance — from the leadership onto the most committed believers; moves restored corporate property, amnesty, political rights, and statehood to the leadership and Utah's integrating economy; and between 1890 and 1904 moves the risk and secrecy burden of new plural sealings onto monogamist members kept ignorant that such sealings continued.
% ABSENT_VOICES: Those entering or performing post-Manifesto sealings had no voice in the public narrative denying their existence; plural wives whose households bore the disruption had no formal seat in General Conference deliberation; the deceived monogamist majority could not object to a gap they were not told existed; and dissenters insisting the mandate remained binding were pushed out of the conversation entirely before they could organize a hearing.
% DISAPPEARANCE_RATIONALE: If the Manifesto arrangement vanished overnight, the federal assault resumes: Edmunds-Tucker dissolution proceeds, escheated property stays forfeited, leadership returns to hiding or prison, statehood fails, and the community either fragments into underground practice or collapses institutionally. The entire subsequent shape of the Mountain West, the church's modern form, and its membership boundaries depend on the arrangement having held.
% FOUNDING_PROBLEM: Existential federal coercion: four decades of escalating statutes culminating in the Edmunds-Tucker Act (1887) disincorporated the church, forfeited its property, disenfranchised polygamists, and threatened the community's legal and physical existence.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: contemporaneous federal officials' correspondence treats the Manifesto as the political settlement Washington had been demanding; historians working from diaries, council minutes, and sealing records document the strategic sequence of concession following escalation; and dissenting apostles who performed post-Manifesto sealings attested that the declaration lacked the revelatory character officially claimed for it. The benefiting parties alone would attest the arrangement as unqualified revelation — that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: substantial but bounded — the arrangement transferred real sacrifice (coerced reversal of a commanded practice, deception of the monogamist majority, eventual discipline of continuers) onto believers while leadership retained office and recovered assets, yet a genuine protective good flowed back to the whole community (prosecutions ended, amnesty, statehood). Suppression 0.70: the arrangement's persistence required active machinery — controlled information channels, selective authorization of secret sealings, and after 1904 disciplinary councils reaching into the Quorum of the Twelve (resignations 1905, excommunication 1911 just past interval end). Theater_ratio 0.50: roughly half of maintenance activity under this reading is narrative performance — the revelation framing, public assurances that the practice had ended, and ceremonial reaffirmation — sustaining the gap rather than performing the suspension's stated function; the ratio rises across the interval as the gap widens and the narrative works harder. Accessibility_collapse 0.50: exit existed (leaving the faith, joining splinter communities, emigration to the colonies) but at severe cost, and alternatives were only partly collapsed — hence middling, not mountain-grade. Resistance 0.55: real and persistent — continued sealings, dissenting apostles, and the later fundamentalist movement — but fragmented by information asymmetry and identity commitments. MEASUREMENTS: all three series run on one shared eight-point grid (1890-1904, biennial); trajectories are monotonic ratchets, not cycles — extraction accumulates as the gap widens, suppression requirement climbs as enforcement hardens toward 1904, theater climbs as the legitimation narrative carries more load. Suppression_requirement is authored because the story specifically traces enforcement-capacity change (from tolerated ambiguity to hardened discipline).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda_setter seat (first_presidency), the arrangement is necessary stewardship: any leader weighing dissolution, imprisonment, and community destruction against suspension of one practice chooses suspension, and the revelatory frame is the only language in which that choice can be announced without shattering the community's commitments. From the payer seats, the same structure operates as betrayal and deception: families who paid everything for a commanded principle are told to stop; the monogamist majority funds an institution concealing ongoing sealings; officiants who acted on the leadership's own signals become criminals of the new line. The federal seat experiences uncomplicated policy success. The engine computes these divergent classifications from the structural data; this story's claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: lds_first_presidency sits nearest the beneficiary pole (controls the narrative, collects survival and assets, arbitrage-grade control of its own exit framing); quorum_of_the_twelve low but slightly offset by the payer secondary_role; federal_government low (collected the outcome without administering the arrangement); utah_non_mormon_residents low with mobile exit damping exposure. Victim declarations drive high directionality: deceived_monogamist_members sit nearest the full-target pole — information asymmetry removes even informed consent, and organized-but-constrained exit amplifies effective extraction; coerced_polygamist_families sit high but below the monogamists, because amnesty and legal protection flow back to them as partial subsidy; post_manifesto_officiants sit high with identity_locked exit amplifying their exposure when enforcement reversed the very permissions they had acted on. Scope is continental-to-global for the leadership seats (harder verification, modest upward scaling of effective extraction) and regional for the trapped victim seats. Suppression is authored as a raw structural property and is deliberately NOT scaled — only extractiveness scales with directionality and scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetric misreadings. A pure-snare reading would erase the genuine coordination achievement — the community demonstrably survived, prosecutions ended, and the aggregate membership net-benefited from the suspension — collapsing a real collective-action solution into mere predation. A pure-rope reading would erase the asymmetric extraction — the deception of the monogamist majority, the coerced reversal borne by those who had sacrificed most, and the discipline of members who trusted the leadership's own signals — laundering extraction as coordination cost. On mandatrophy proper: the founding problem (existential federal coercion) was resolved by 1896 (amnesty, statehood, property restoration), yet the arrangement persists — canonized as Official Declaration 1, with its enforcement machinery intact well past the interval — and its maintenance grows more theatrical as the survival function becomes historical. The R5 mismatch signature (founding_problem_status=dead x disappearance_verdict=world_rearranges) flags precisely this zombie persistence: the arrangement no longer solves its founding problem but its removal still rearranges the world, because the institution's current shape, narrative, and membership boundaries depend on it. Identity-lock dynamics bind the agenda_setter seat: the presidential office IS the oracle, so admitting the revelation narrative was strategic legitimation would dissolve the authority that administers it — professional-institutional fusion, not mere career dependence. Had that frame broken in 1890-1904, the arrangement would have collapsed into either open schism or honest doctrinal revision; instead the interpretive layer absorbed the gap for fourteen years before enforcement forced closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading correctly characterizes the 1890 Manifesto — strategic adaptation legitimated by a revelation narrative (this reading), genuine prophetic reinterpretation (endogenous_reinterpretation_reading), or pure exogenous override of a still-binding requirement (exogenous_override_reading)? The disagreement is located in the causal locus of the decision (strategic calculation versus revelatory event) and in the referential status of the revelation claim.',
    'Triangulate Wilford Woodruff''s dated diary entries, council minutes, and the sequencing of concessions against contemporaneous federal demands; compare the decision record with the later retrospective revelation framing.',
    'Adopting the endogenous reading collapses the deceived-monogamist victim class and drops measured extraction toward coordination cost; adopting the exogenous reading raises suppression (externally imposed) and restructures the beneficiary set. This file''s epsilon and type hold only under the pragmatist characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one reading of kernel plural_marriage_mandate; sibling readings instantiate different constraints with different epsilon and victim structure.').

omega_variable(
    post_manifesto_sealing_extent,
    'How many plural sealings were performed between 1890 and 1904, under whose authorization, and how widely was the continuation known or suspected among the general membership?',
    'Reconstruction from temple sealing records, missionary and colony registers (Mexico, Canada), and the prosopography compiled in the scholarly literature on post-Manifesto marriages.',
    'Extensive continuation raises effective extraction on deceived_monogamist_members and confirms the doctrine-practice gap as the primary observable; near-zero continuation collapses that victim class and lowers the deception component of extractiveness substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_sealing_extent, empirical, 'Extent and knowledge distribution of the 1890-1904 secret continuations.').

omega_variable(
    woodruff_vision_sincerity,
    'Did Woodruff''s reported visions precede and cause the decision to suspend, or were they constructed or elaborated retrospectively to legitimate a decision already reached on strategic grounds?',
    'Date-stamped diary entries versus later retellings; contemporaneous private correspondence with counselors and apostles during the negotiation period.',
    'A sincere, prior, decision-causing vision pushes the constraint toward the endogenous reading and reattributes much of the measured extraction to genuine coordination cost; retrospective construction confirms this reading''s legitimation claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(woodruff_vision_sincerity, conceptual, 'Sincerity and timing of the revelatory account relative to the strategic decision.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression among believing members structural (disciplinary machinery, economic dependence on church employment and community) or internalized (obedience-to-prophets identity making questioning the declaration unthinkable)?',
    'Post-1904 trajectory comparison: members disciplined for continued sealings versus those who left voluntarily; persistence of deference patterns after enforcement capacity declined in the 1910s.',
    'If substantially internalized, effective suppression exceeds the structural measure and predicts persistence of compliance after enforcement decay; if structural, suppression should track the enforcement machinery''s rise and fall directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized components of member compliance with the suspension.').

omega_variable(
    survival_motive_vs_positional_rent,
    'Was the leadership''s operative motive institutional survival of the whole community (protecting members from prosecution and the church from dissolution), or preservation of positional authority and corporate assets (rent retention)?',
    'Examine whether leadership weighed resignation, decentralization, or voluntary disestablishment alternatives; compare outcomes for contemporaneous groups that refused capitulation; trace asset restoration sequencing against member-protection sequencing.',
    'Survival-dominant motive supports the tangled_rope classification (genuine coordination function entangled with asymmetric costs); rent-dominant motive pushes the computed classification toward snare and recasts the beneficiary structure as pure capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_motive_vs_positional_rent, empirical, 'Motivational basis distinguishing coordination-side from extraction-side operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.34).
narrative_ontology:measurement(plur_tr_t1892, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1892, 0.37).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.39).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.41).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.43).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.48).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.5).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(plur_be_t1892, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1892, 0.57).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.59).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.62).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.64).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.67).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.5).
narrative_ontology:measurement(plur_su_t1892, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1892, 0.53).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.56).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.58).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.61).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1900, 0.63).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.66).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims that decompose per the epsilon-invariance principle. This story (institutional_pragmatism_reading) authors epsilon for the standing Manifesto arrangement assessed by pragmatist lights — moderately-high extraction via deception and coerced sacrifice entangled with genuine survival coordination (tangled_rope). The exogenous_override_reading authors the same arrangement as externally imposed abandonment of a binding requirement (different suppression profile, different victim structure: the requirement itself is the victim). The endogenous_reinterpretation_reading authors it as legitimate prophetic action (low extraction, near-rope). Upstream/downstream: the documentary record of federal coercion (exogenous) supplies the factual substrate this reading interprets strategically; this reading in turn supplies the critical apparatus that the endogenous reading must answer. All three files link one another via network.affects_constraints; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
