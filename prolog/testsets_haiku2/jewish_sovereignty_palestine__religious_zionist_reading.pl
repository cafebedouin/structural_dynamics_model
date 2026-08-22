% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Promise of Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/nationalism/theology
 *
 * SUMMARY:
 *   The religious Zionist reading grounds Jewish statehood in Palestine
 *   (Eretz Yisrael) in a covenant claim: God promises the land eternally to
 *   the Jewish people, making territorial sovereignty not merely legitimate
 *   but theologically mandated. Statehood is experienced as restoration and
 *   fulfillment of this divine obligation, not negotiable through
 *   international law or demographic consent. This reading is instantiated as
 *   one of five competing interpretations of the jewish_sovereignty_palestine
 *   kernel. The constraint's referent is the standing arrangement—the claim
 *   of Jewish inalienable right to the land and its corollary rejection of
 *   Palestinian self-determination at equal weight—assessed from within the
 *   religious Zionist reading's own theological and interpretive framework.
 *   The reading's endorsed alternative (a binational, secular, or negotiated
 *   partition arrangement) is NOT the referent; ε measures the extractiveness
 *   of the standing theological claim itself.
 *
 * KEY AGENTS:
 *   - Jewish people (covenant community): Identity-locked beneficiary; the reading constitutes their peoplehood through the covenant claim.
 *   - Palestinian Arabs: Constrained payer; their displacement and subordination in the normative calculus is the cost structure of the reading.
 *   - Eretz Yisrael (the land): Non-agent entity with theological destiny in the reading; restoration/redemption of the land is the goal.
 *   - International law framework: Excluded institutional observer; its competing principles (self-determination, territorial integrity) are structurally incompatible with the religious reading.
 *   - Secular Jewish Israelis: Beneficiary + payer; benefit from statehood but constrained by the theological absolutism and land-maximalism the reading entails.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.89).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.76).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, mountain).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Promise of Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/nationalism/theology").

domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '2936be7a-f1e1-4f2a-9492-4f0aedad0540').
narrative_ontology:cs_kernel_codification('2936be7a-f1e1-4f2a-9492-4f0aedad0540', fixed_text).
narrative_ontology:cs_authority_grounding('2936be7a-f1e1-4f2a-9492-4f0aedad0540', lineage).
narrative_ontology:cs_interpretation_layer_present('2936be7a-f1e1-4f2a-9492-4f0aedad0540').
narrative_ontology:cs_reading_relation('2936be7a-f1e1-4f2a-9492-4f0aedad0540', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2936be7a-f1e1-4f2a-9492-4f0aedad0540', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('2936be7a-f1e1-4f2a-9492-4f0aedad0540', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('2936be7a-f1e1-4f2a-9492-4f0aedad0540', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('2936be7a-f1e1-4f2a-9492-4f0aedad0540', foundational, divine_covenant_eternally_binding).
narrative_ontology:cs_axiom_status(divine_covenant_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('2936be7a-f1e1-4f2a-9492-4f0aedad0540', divine_covenant_eternally_binding, theological).
narrative_ontology:cs_axiom('2936be7a-f1e1-4f2a-9492-4f0aedad0540', foundational, eretz_yisrael_non_negotiable_jewish_right).
narrative_ontology:cs_axiom_status(eretz_yisrael_non_negotiable_jewish_right, holdable).
narrative_ontology:cs_axiom_grounding('2936be7a-f1e1-4f2a-9492-4f0aedad0540', eretz_yisrael_non_negotiable_jewish_right, deontological).
narrative_ontology:cs_reference_frame('2936be7a-f1e1-4f2a-9492-4f0aedad0540', torah_covenant_promise_as_fixed_kernel).
narrative_ontology:cs_drift_state('2936be7a-f1e1-4f2a-9492-4f0aedad0540', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2936be7a-f1e1-4f2a-9492-4f0aedad0540', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_covenant_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, secular_jewish_israelis).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, secular_jewish_israelis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Understood as the covenanted recipients of a divine promise binding them to Eretz Yisrael (the Land of Israel). The religious Zionist reading treats this promise as eternally valid, non-negotiable, and constitutive of Jewish peoplehood itself. Statehood is experienced as fulfillment of theological obligation and restoration of rightful sovereignty. Exit from this claim would require renouncing core religious identity and historical narrative.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_covenant_community, beneficiary,
    organized, civilizational, identity_locked, universal).

% The land itself is treated in this reading as a non-agent entity that is restored, redeemed, and brought into its divinely intended state through Jewish sovereignty and settlement. The land has a theological destiny independent of current demographic or political arrangements.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, eretz_yisrael_land, beneficiary,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(jewish_sovereignty_palestine__religious_zionist_reading, eretz_yisrael_land).

% Benefit from the existence of a Jewish state as a refuge, symbol of Jewish survival and self-determination, and spiritual center. Their connection to the claim is real but mediated through distance and choice; they can decline to immigrate or engage without ceasing to be Jewish, though the reading suggests they are spiritually impoverished by doing so.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, mobile, global).

% In this reading, Palestinians are largely absent from the calculation of legitimacy or cost. Where they appear, they are positioned as secondary occupants whose presence does not invalidate the Jewish people's divine right to the land. Their claims to self-determination and territorial sovereignty are subordinated to or rendered incommensurable with the theological claim. They bear the concrete cost of displacement, fragmentation, and limited sovereignty, but the reading does not recognize this cost as a structural feature—it treats Palestinian presence as a practical problem to be managed, not a party with equal standing in the normative claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_arabs, payer,
    moderate, generational, constrained, regional).

% International law frameworks (UN resolutions, Geneva conventions, ICC jurisdiction) assert competing principles: self-determination for Palestinians, territorial integrity, the illegality of territorial acquisition by force, return of refugees. These frameworks are explicitly rejected or reinterpreted in the religious Zionist reading as inapplicable to a covenant claim. International institutions would argue for partition, negotiated two-state solutions, and equal weight to Palestinian self-determination; they are structurally excluded from the religious reading's normative system.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_law_framework, excluded,
    institutional, generational, constrained, universal).

% Benefit from Jewish statehood and security but do not endorse or may actively contest the theological grounding of the territorial claim. They experience the constraint as beneficial (statehood, self-determination) but may resist its theological absolutism and the land-maximalism it entails. Many advocate for negotiated territorial compromise; they are not targets of the constraint but their secular framing sits in tension with the reading's theological necessity claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_jewish_israelis, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, secular_jewish_israelis, payer).

% Historical scholarship, postcolonial theory, international relations analysis observe and contest the reading's claims. They examine the historical settlement patterns, the theological interpretation's invention or continuity, the relationship between religious narrative and political claim, and the asymmetry in how Jewish and Palestinian narratives are weighted. Their analysis is external to the reading's own normative framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_observer_community, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, jewish_people_covenant_community).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Jewish diaspora communities across centuries and continents around a shared spiritual center, collective identity, and theological destiny. Provides a focal point for Jewish learning, culture, and refuge. Creates a framework for interpreting Jewish history and suffering as part of a cosmic narrative of exile and return.
% TRANSFER_FUNCTION: Moves territorial control, political sovereignty, and demographic primacy from other claimants to Jewish political authority. Transfers moral legitimacy of the land itself from secular/international-law frameworks to theological covenant frameworks. In this reading, Palestinians are not recognized as equal parties whose claims must be negotiated; their displacement is a necessary cost of fulfilling the covenant, not an injustice requiring remedy.
% ABSENT_VOICES: Palestinian national movements, secular internationalist critics, postcolonial scholars, and international law advocates would fundamentally reject the framing if admitted. They argue Palestinian self-determination has equal weight, that international law supersedes theological claim, that secular statehood is sufficient and ethical, and that the theological framing serves to exempt the constraint from ordinary moral scrutiny. The reading structurally excludes them by treating the covenant as primary and their objections as secondary or incommensurable.
% DISAPPEARANCE_RATIONALE: Religious Zionists argue the covenant cannot disappear—it is eternal and God's will; if the state dissolved, the obligation would persist and Jewish history would be incomplete. International observers and Palestinian advocates argue the territorial constraint could be replaced by secular two-state coexistence, international law-based partition, or binational democratic arrangements; the world would reorganize around mutual recognition and legal equality rather than theological claim.
% FOUNDING_PROBLEM: Jewish diaspora vulnerability: scattered Jewish communities face existential persecution and lack refuge or political power to ensure survival and dignity. Theology: Jews are called by covenant to return to and restore sovereignty in Eretz Yisrael, fulfilling their divine purpose and finding enduring security.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist authorities (rabbinical tradition, contemporary religious nationalist voices, Israeli right-wing political leadership) attest the covenant is eternally binding and the founding problem of exile and vulnerability persists. Secular observers and international advocates attest the practical founding problem (diaspora refuge-seeking) was addressed by statehood itself, independent of theological maximalism, and that continued religious framing now obscures the need for negotiated coexistence with Palestinians. The corroboration splits along reading-lines; those outside the religious framework do not accept the theological corroboration as legitimate.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, contested).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_sovereignty_palestine__religious_zionist_reading),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type is MOUNTAIN: the reading asserts that the divine promise is a natural/irreducible fact of reality, not a constructed human arrangement. However, the metrics authored descriptively are those of a heavily extractive, suppressed, and theatrically maintained constraint. This gap—CLAIMED mountain, MEASURED as extractive—is exactly the false-summit signature: a constraint presented as natural law but operating with high extraction and active suppression. The extractiveness score of 0.89 reflects the reading's subordination of Palestinian self-determination claims and its claim to non-negotiability. Suppression of 0.76 reflects the enforcement required to maintain demographic dominance and prevent competing claims (international law, Palestinian nationalism) from reframing the terms. Theater ratio of 0.42 reflects that a significant share of the constraint's maintenance activity is devoted to theological justification and narrative legitimation rather than pure administration—the constant rehearsal of covenant, return, and restoration narratives. Accessibility collapse of 0.72 reflects that within the reading's own framework, alternatives (partition, binational state, Palestinian equality) are largely rendered unthinkable or illegitimate. Resistance of 0.88 is very high because the constraint meets sustained, organized resistance from Palestinian liberation movements, international law advocates, and secular Jewish critics. The measurement series tracks gradual intensification from t=0 (early statehood, 1948) through t=76 (present day): extractiveness and suppression both rise as settlement expands and the theological maximalism is more extensively institutionalized; theater rises modestly as the constraint's maintenance increasingly relies on narrative and ideological work.
 *
 * PERSPECTIVAL GAP:
 *   The religious Zionist seat (beneficiary) experiences the constraint as natural law, divine obligation, and restoration—a mountain whose disappearance would mean cosmic disorder and unfulfilled destiny. The Palestinian seat (payer) experiences the same constraint as an enforced displacement regime, a settler-colonial instantiation presented under theological cover, its suppression systematic. The secular Israeli seat experiences it as beneficial (security, statehood) but resists its absolutism. The international observer seat experiences it as a contested political claim using theological framing to escape legal and ethical scrutiny. The engine computes each seat's type from the power/exit/beneficiary/victim data; this structural asymmetry is why a mountain claimed from one reading computes as tangled_rope or snare from another seat's structural position. The religious reading's own internal logic—that the claim is natural, non-negotiable, and absolute—is what makes it a mountain under its own lights; the measurement metrics descriptively capture how that claim is actually maintained and what costs it imposes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people (covenant community) are the declared beneficiary and are identity-locked to the claim—they cannot exit without ceasing to identify with the religious tradition that constitutes them. Their directionality is near 0.0 (full beneficiary, zero extraction). Palestinians are the implicit target (payer in victims[]), constrained in exit options, and their structural position derives extraction from them to benefit the Jewish state. However, in the religious Zionist reading's own logic, Palestinians are not fully recognized as parties with equal standing; they appear in the calculation as an obstacle to be managed rather than a structural opponent. This asymmetry—the reading structurally excludes Palestinian claims from the beneficiary/victim calculus itself—is the deepest extraction mechanism: the cost is imposed but the legitimacy framework does not require Palestinian consent or even recognition of Palestinian agency. The engine will derive Palestinian directionality from their constrained exit and victim classification, but the reading itself attempts to render them incommensurable with the framework's normative logic. This is the false-summit dynamic: a claim to natural-law status that actually depends on high institutional suppression and the denial of competing claims' legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora vulnerability and Jewish refugee need) was substantially solved by the establishment of the state and the creation of a refuge open to Jewish immigration. The religious Zionist reading's persistence depends on reframing the founding problem as not merely solved but as eternally present—the people are always at risk, always in exile from their true land, always needing theological restoration. This is classic mandatrophy: a founding mandate (ensure refugee security) becomes a permanent feature because the framing that made it necessary is now institutionalized in the theological claim. The constraint no longer needs to justify itself as a response to diaspora emergency (that emergency is in the past or exaggerated); it justifies itself as fulfillment of eternal covenant. This is a mountain only if the covenant is actually natural law; it is a piton with mountain cover if the covenant is a human interpretive claim that the constraint's maintenance machinery treats as settled natural fact. An omega variable addresses this irreducible ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_as_natural_law_vs_constructed_claim,
    'Is the divine covenant to the Jewish people a natural-law fact transcending human interpretation, or a constructed theological-political claim whose status depends on ongoing communal and institutional maintenance?',
    'Test for mountain-ness: (a) does the claim persist unchanged if enforcement machinery is removed? (b) does the claim''s status depend on active suppression of competing interpretations? (c) has the theological interpretation itself evolved in response to political pressures? If (b) and/or (c) are true, the claim is not natural law. Examine historical records of rabbinical interpretation, settlement ideology development, and adaptation of covenant theology to justify territorial expansion.',
    'If the covenant is natural law (mountain), the false-summit signature is illusory and the constraint is genuinely non-negotiable. If the covenant is a constructed claim maintained through theological labor, the constraint is tangled_rope (coordination + extraction), and alternatives (secular statehood, negotiated partition) become thinkable within the Jewish tradition itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_as_natural_law_vs_constructed_claim, conceptual, 'Whether the religious covenant claim is a discovered natural fact or a continuously reconstructed interpretive tradition.').

omega_variable(
    palestinian_absence_vs_subordination,
    'Is Palestinian absence from the beneficiary/victim calculus in this reading a structural feature (they have no standing in the framework) or a rhetorical choice (they are present but systematically downweighted)?',
    'Examine theological texts and political rhetoric: do they assert Palestinians have no moral standing, or do they acknowledge Palestinian claims but assign them lower priority? Do they claim the land is empty or claim Jewish right supersedes Palestinian right? The distinction determines whether the constraint is mountain-framed (denying incommensurable parties standing) or snare-framed (extracting from acknowledged targets).',
    'Structural absence (they have no standing) suggests the reading is internally coherent as a mountain from its own seat. Systematic subordination (they have standing but lower weight) suggests the reading is a snare dressed as a mountain—acknowledging target existence but denying their equality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_absence_vs_subordination, empirical, 'Whether Palestinians are treated as incommensurable parties or as lower-priority claimants within the religious Zionist framework.').

omega_variable(
    false_summit_detection_candidate,
    'The constraint is claimed as mountain but measured as highly extractive with substantial suppression. Is this a false summit—a constructed constraint presented as natural law to benefit identifiable parties—or is the high extractiveness the cost of maintaining a genuinely natural-law claim against determined resistance?',
    'False-summit test (schema enforcement): Mountains with declared beneficiaries must carry an omega documenting the ambiguity. This is that omega. If beneficiaries (jewish_people_covenant_community) are genuinely benefiting from a natural-law constraint, why does the constraint require (a) high suppression, (b) active enforcement against alternatives, (c) continuous theological justification? Natural laws (gravity, logic) require no enforcement. The presence of enforcement implies either: (1) false summit—a constructed claim falsely framed as natural, or (2) genuine natural law meeting resistance from those it extracts from (Palestinians) who dispute its legitimacy. The resolution is historical and comparative: examine whether this pattern (high enforcement + mountain claim) appears in other false-summit cases (e.g., labor as natural hierarchy, gender roles as natural order). If the pattern is systematic, it is false-summit signature.',
    'Confirmation of false-summit status would trigger the engine''s false_summit_mountain signature override and reclassify the constraint from mountain to tangled_rope or snare. The classification would shift from ''natural law'' to ''constructed extraction legitimized through natural-law framing.'' This would not refute the religious Zionist reading''s truth-claims about the covenant; it would clarify that the covenant, if true, operates as a constraint through active institutional maintenance and suppression, not through unforced natural persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detection_candidate, empirical, 'FSM candidate: mountain-claimed constraint with beneficiaries, high extractiveness, substantial suppression, and high resistance. Diagnostic for constructed constraints masquerading as natural law.').

omega_variable(
    sibling_reading_incommensurability,
    'Can the religious Zionist reading logically coexist in a single framework with the settler_colonial reading, or do they foreclose each other?',
    'The settler_colonial reading denies any non-constructed basis for Jewish claim; it asserts the claim is a European political ideology wearing religious vestments. The religious Zionist reading asserts the claim is grounded in divine promise, eternally binding. These are logically opposed IF the covenant is assumed real; they coexist only if covenant truth is left undecided. Examine whether contemporary adherents of each reading acknowledge the logical opposition or attempt to compartmentalize (one reads religious truth, the other political structure, both treating the other''s domain as orthogonal).',
    'If they foreclose each other, the reading_relations should be forecloses, not coexists_with. If they coexist through compartmentalization (religious truth vs. political causation are different axes), then coexists_with is correct and the kernel itself remains contested—no single framework can hold both, but different frameworks do hold each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incommensurability, conceptual, 'Whether the religious Zionist and settler-colonial readings are logically incompatible or coexist by occupying different explanatory domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t0, observed).
narrative_ontology:measurement(jewi_tr_t8, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t8, observed).
narrative_ontology:measurement(jewi_tr_t16, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(jewi_tr_t16, observed).
narrative_ontology:measurement(jewi_tr_t28, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement_basis(jewi_tr_t28, observed).
narrative_ontology:measurement(jewi_tr_t52, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 52, 0.41).
narrative_ontology:measurement_basis(jewi_tr_t52, observed).
narrative_ontology:measurement(jewi_tr_t76, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 76, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(jewi_be_t0, observed).
narrative_ontology:measurement(jewi_be_t8, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 8, 0.76).
narrative_ontology:measurement_basis(jewi_be_t8, observed).
narrative_ontology:measurement(jewi_be_t16, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 16, 0.81).
narrative_ontology:measurement_basis(jewi_be_t16, observed).
narrative_ontology:measurement(jewi_be_t28, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 28, 0.85).
narrative_ontology:measurement_basis(jewi_be_t28, observed).
narrative_ontology:measurement(jewi_be_t52, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 52, 0.87).
narrative_ontology:measurement_basis(jewi_be_t52, observed).
narrative_ontology:measurement(jewi_be_t76, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 76, 0.89).
narrative_ontology:measurement_basis(jewi_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement_basis(jewi_su_t0, observed).
narrative_ontology:measurement(jewi_su_t8, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(jewi_su_t8, observed).
narrative_ontology:measurement(jewi_su_t16, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 16, 0.69).
narrative_ontology:measurement_basis(jewi_su_t16, observed).
narrative_ontology:measurement(jewi_su_t28, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement_basis(jewi_su_t28, observed).
narrative_ontology:measurement(jewi_su_t52, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 52, 0.74).
narrative_ontology:measurement_basis(jewi_su_t52, observed).
narrative_ontology:measurement(jewi_su_t76, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 76, 0.76).
narrative_ontology:measurement_basis(jewi_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__religious_zionist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The jewish_sovereignty_palestine kernel admits five distinct constraint stories, each representing a reading of the same contested commitment (Jewish right to territory in Palestine/Eretz Yisrael) from a different normative and epistemic position. The religious_zionist_reading instantiated here treats the claim as grounded in divine covenant (natural law, mountain-claimed). The settler_colonial_reading treats the same territorial arrangement as a displacement regime (snare). The liberal_nationalist_reading grounds the claim in secular self-determination rights (rope). Each reading has its own ε, its own beneficiary/victim structure, its own classification. They are linked here because they share the kernel—the contested legitimacy basis—and because changes in one reading's authority or credibility cascade to affect the others. The decomposition follows the ε-invariance principle: a single kernel but multiple structurally distinct constraints, each with its own metrics, because the reading's own lights change what counts as extraction and who counts as a party.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__religious_zionist_reading, moderate, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
