% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__credentialed_expertise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__credentialed_expertise_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_knowledge_boundary__credentialed_expertise_reading
 *   human_readable: Credentialed Expertise Gatekeeping: Legitimate Knowledge Boundary (Expertise Reading)
 *   domain: epistemology/science/technology/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the credentialed-expertise reading of
 *   the contested kernel 'legitimate knowledge boundary.' Under this reading,
 *   legitimate knowledge is the knowledge that emerges from methodologically
 *   rigorous inquiry—empirical testing, hypothesis-falsification,
 *   quantification, systematic documentation—validated through credentialed
 *   peer review. The constraint operates by gatekeeping: only researchers
 *   with advanced credentials, institutional affiliation, and successful
 *   passage through peer review get presumptive authority. The reading treats
 *   this gatekeeping as functional (protecting against charlatanism and
 *   opinion-as-fact) and necessary. Sibling readings reject this framing: the
 *   experiential-pluralism reading emphasizes knowledge validated through
 *   lived experience and community practice; the hybrid-coproduction reading
 *   argues both credentialed and experiential validation are necessary and
 *   that methodological rigor should be pluralized. This constraint story
 *   describes the world as the credentialed-expertise reading understands it:
 *   a real coordination function protecting knowledge quality, with some
 *   asymmetric distribution of authority as an unavoidable cost. The engine
 *   will compute whether that claim survives the metrics and the structural
 *   data.
 *
 * KEY AGENTS:
 *   - Credentialed academic institutions: set standards, control funding, certify credentials — primary beneficiaries
 *   - Peer-review gatekeepers: adjudicate publication, control prestige venues — agenda-setter and secondary beneficiary
 *   - Credentialed disciplinary experts: enjoy presumptive authority and career access — primary beneficiaries
 *   - Non-credentialed knowledge practitioners: excluded from legitimate-knowledge arena, identity-locked — primary victims
 *   - Excluded epistemologies (traditional medicine, Indigenous knowledge, craft mastery): systematically devalued, excluded from policy and funding — primary victims
 *   - Experiential knowledge communities: constrained to credentialed language to gain hearing — secondary victims
 *   - Policy makers and funding bodies: benefit from treating expert consensus as truth-proxy — secondary beneficiaries
 *   - Methodological pluralists and critics: excluded by gatekeeping they critique — excluded stakeholders
 *   - Observer: STS and philosophy-of-science scholars document the constraint's operation — analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68).
domain_priors:suppression_score(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.71).
domain_priors:theater_ratio(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__credentialed_expertise_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__credentialed_expertise_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__credentialed_expertise_reading, "Credentialed Expertise Gatekeeping: Legitimate Knowledge Boundary (Expertise Reading)").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__credentialed_expertise_reading, "epistemology/science/technology/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__credentialed_expertise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e93e5206-990b-4f24-8498-e988983039da').
narrative_ontology:cs_kernel_codification('e93e5206-990b-4f24-8498-e988983039da', fixed_text).
narrative_ontology:cs_authority_grounding('e93e5206-990b-4f24-8498-e988983039da', extraction).
narrative_ontology:cs_interpretation_layer_present('e93e5206-990b-4f24-8498-e988983039da').
narrative_ontology:cs_reading_relation('e93e5206-990b-4f24-8498-e988983039da', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_reading_relation('e93e5206-990b-4f24-8498-e988983039da', legitimate_knowledge_boundary__hybrid_coproduction_reading, influences).
narrative_ontology:cs_axiom('e93e5206-990b-4f24-8498-e988983039da', foundational, methodological_rigor_epistemically_primary).
narrative_ontology:cs_axiom_status(methodological_rigor_epistemically_primary, holdable).
narrative_ontology:cs_axiom_grounding('e93e5206-990b-4f24-8498-e988983039da', methodological_rigor_epistemically_primary, empirically_contingent).
narrative_ontology:cs_axiom('e93e5206-990b-4f24-8498-e988983039da', foundational, credentialed_expertise_institutional_necessary).
narrative_ontology:cs_axiom_status(credentialed_expertise_institutional_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e93e5206-990b-4f24-8498-e988983039da', credentialed_expertise_institutional_necessary, instrumental).
narrative_ontology:cs_axiom('e93e5206-990b-4f24-8498-e988983039da', secondary, peer_consensus_truth_proxy).
narrative_ontology:cs_axiom_status(peer_consensus_truth_proxy, holdable).
narrative_ontology:cs_axiom_grounding('e93e5206-990b-4f24-8498-e988983039da', peer_consensus_truth_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('e93e5206-990b-4f24-8498-e988983039da', methodological_rigor_as_primary_legitimacy).
narrative_ontology:cs_drift_state('e93e5206-990b-4f24-8498-e988983039da', contemporary_open_science_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e93e5206-990b-4f24-8498-e988983039da', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_institutions).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeepers).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, methodologically_aligned_disciplines).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_practitioners).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, excluded_epistemologies).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_disciplinary_experts).
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Universities, research institutes, and national science academies set and enforce standards for what counts as legitimate knowledge. They control funding allocation, hiring, publication pathways, and credential certification. They justify these standards as necessary for quality assurance and methodological rigor; they simultaneously benefit from exclusive control of the 'legitimate knowledge' label.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Editors, journal publishers, and credentialed reviewers adjudicate what research gets published and thus what enters the legitimate-knowledge canon. They control access to the prestige venues through which careers are built. Their gatekeeping role is justified as quality control; they also derive authority, prestige, and economic rent from scarcity of publication slots.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeepers, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, peer_review_gatekeepers, beneficiary).

% Researchers with advanced degrees and institutional affiliation benefit from exclusive access to credibility, funding, and platform. Their claims carry presumptive authority; their methods are treated as standard; their consensus is treated as truth-proxy. They can exit the constraint by changing fields or leaving academia, but doing so costs career capital.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_disciplinary_experts, beneficiary,
    powerful, biographical, mobile, global).

% Indigenous scholars, community-based researchers, practitioners of traditional medicine and ecological knowledge, autodidacts, and professionals outside academic hierarchies cannot easily enter the legitimate-knowledge arena. Their claims require translation into credentialed language to be heard; their methods are scrutinized asymmetrically; they bear the burden of proof credentialed researchers do not face. They are identity-locked because legitimacy of their knowledge is often fused with identity: to gain credibility, they must partially assimilate to credentialed norms.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, non_credentialed_knowledge_practitioners, payer,
    powerless, biographical, identity_locked, global).

% Ways of knowing that privilege different validation methods—spiritual knowledge systems, craft mastery, intuitive insight, collective memory, ecological observation accumulated across generations—are systematically devalued as non-rigorous. Practitioners of these epistemologies pay by having their knowledge excluded from policy, funding, and public authority, even where their track record is centuries long. Their exit is structural: the constraint's enforcement mechanism is what traps them (the definition of legitimate knowledge is what excludes them).
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, excluded_epistemologies, payer,
    moderate, generational, trapped, global).

% Communities validating knowledge through collective practice—farmers, healers, environmental monitors, repair practitioners—accumulate empirical understanding that works reliably over time. They are constrained by having to justify this knowledge in credentialed terms to access resources or policy influence, even though their knowledge often outperforms methodological-only approaches on local optimization problems.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_communities, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimate_knowledge_boundary__credentialed_expertise_reading, experiential_knowledge_communities, excluded).

% Government and private funding sources (NIH, NSF, Gates Foundation, etc.) institutionalize the credentialed-expertise standard by directing resources only to credentialed researchers and institutions. They thus reinforce the constraint's enforcement and benefit from having a clear, unambiguous criterion for legitimate research claims.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, funding_bodies, agenda_setter,
    institutional, generational, analytical, national).

% Government and organizational decision-makers benefit from treating peer-reviewed expert consensus as truth-proxy: it simplifies policy choice and outsources accountability to 'the experts.' They face lower costs of policy error and higher legitimacy when they can cite credentialed expertise, even when that expertise is contested or incomplete.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, policy_makers, beneficiary,
    powerful, biographical, mobile, national).

% Researchers and theorists who argue for methodological pluralism or epistemological humility—that different research questions require different methods and that credentialed expertise is one valuable input among others—are excluded from the conversation by being labeled non-rigorous or postmodern. Their arguments are heard but delegitimated through the same gatekeeping process they critique.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, methodological_pluralists, excluded,
    moderate, biographical, constrained, global).

% Scholars in science and technology studies (STS), philosophy of science, and critical epistemology study how knowledge legitimacy is constructed and enforced. They document the constraint's operation without direct stake in its outcome.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__credentialed_expertise_reading, observer_epistemologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__credentialed_expertise_reading, credentialed_academic_institutions).
narrative_ontology:fixing_cost_class(legitimate_knowledge_boundary__credentialed_expertise_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a shared validation standard connecting distributed researchers across institutions, disciplines, and time: peer review, methodology, credentialing. Solves the problem of cumulative knowledge building in the absence of central authority. Enables policy-makers to trust expert consensus without themselves doing the research.
% TRANSFER_FUNCTION: Moves funding, prestige, authority, and policy influence from non-credentialed practitioners to credentialed institutions and gatekeepers. Moves epistemic authority from communities of practice to disciplinary experts. Transfers opportunity cost: credentialed researchers can operate under presumption of legitimacy; non-credentialed practitioners must fight for standing at every claim.
% ABSENT_VOICES: Indigenous scholars and knowledge keepers who would argue their epistemologies have generated validated, reliable knowledge for centuries and are excluded not because they lack rigor but because rigor looks different within their epistemic frameworks. Community-based and experiential practitioners who would argue their knowledge is tested through use over long time horizons and is more reliable for local optimization than methodologically standardized approaches. Craft masters, traditional healers, ecological observers who would argue their validation methods work empirically but are devalued because they do not fit credentialed formats.
% DISAPPEARANCE_RATIONALE: If the credentialed-expertise boundary vanished overnight, research funding would be distributed differently (alternative validation mechanisms would compete with peer review); policy-making would pluralize epistemological inputs; academic hierarchies would be flattened; career pathways would fragment into multiple validation tracks rather than the single credentialed track. The unified truth-proxy function—'ask the credentialed experts'—would dissolve into multiple overlapping knowledge ecosystems with different validation methods. This is too large a restructuring to be absorbed without institutional rearrangement.
% FOUNDING_PROBLEM: Early-modern and pre-modern inquiry had no systematic mechanism to distinguish reliable knowledge from opinion dressed in rhetoric, or to accumulate knowledge across researchers who had never met. Authority was personal (the master's reputation) or ideological (the tradition's dogma). Charlatans and ideologues could capture epistemic authority through eloquence or patronage, regardless of warrant. The founding problem was: how can knowledge validation scale beyond personal networks and ideological tribes, such that distributed inquirers can trust findings they did not personally verify?
% FOUNDING_PROBLEM_CORROBORATION: The credentialed-expertise reading attests the founding problem is still live: peer review and credentialing are still necessary to prevent fraud and distinguish signal from noise at scale. Critics from the experiential-pluralism and hybrid-coproduction readings attest the founding problem was substantially solved by mid-20th century and peer review is now a bottleneck and boundary-maintenance mechanism: obvious fraud is still caught, but the credential inflation, the publication gatekeeping, and the exclusion of alternative validation methods are now extractive rather than protective. Historical analysis and the success of non-credentialed validation mechanisms (open-source code validation, prediction markets, citizen science producing research-grade datasets) corroborate the shifted-function reading from independent observers outside the credentialed-institution seat.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__credentialed_expertise_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__credentialed_expertise_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__credentialed_expertise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__credentialed_expertise_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__credentialed_expertise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint creates a large structural asymmetry: credentialed researchers can publish, gain funding, and influence policy; non-credentialed practitioners of equivalent knowledge must gain translation into credentialed norms or remain unheard. The extraction is not just material (funding flows to credentialed institutions) but epistemic (who gets to be heard, who gets to define what counts as knowledge). Suppression is also high (0.71) because the constraint's persistence depends on actively excluding alternative epistemologies and enforcing credential inflation—new barriers to entry keep non-credentialed practitioners perpetually outside. Theater is moderate-high (0.42) because a growing share of peer-review activity is not quality control but boundary-maintenance: the constraint must continuously defend against challenges from open-source knowledge, citizen science, Indigenous scholarship, and experiential validation. Accessibility collapse is high (0.73) because once someone is outside the credentialed system, alternatives are extremely limited: they must either acquire credentials (which requires time, resources, and cultural adaptation) or accept non-legitimacy. The measurement series shows extraction and suppression accumulating over the interval (50 years): as credential inflation accelerates, as publishing costs rise, and as policy increasingly requires peer-reviewed evidence, the constraint tightens. Theater rises early then plateaus, suggesting the performative maintenance (defending the boundary against methodological pluralists, explaining why other epistemologies are less valid) becomes standard practice. The coercion grid shows asymmetric tightening across levels: at the organizational level (universities, publishers, funding bodies), suppression rises steeply; at the individual level it rises more slowly (individual researchers can sometimes work around the constraint, or have exit options credentialed gatekeepers do not). The class level shows resistance rising alongside suppression: non-credentialed knowledge communities are mounting more organized challenge. This asymmetry is structural: the constraint can suppress individuals while struggling to suppress organized class-level resistance.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute drastically differently. From the academic-institution seat (agenda-setter), the arrangement is a genuine coordination mechanism: shared standards reduce information asymmetry, enable knowledge accumulation, protect against charlatanism. The measured extraction is real but justified as coordination cost. From the non-credentialed-practitioner seat (payer), the same arrangement is enforced gatekeeping: a ceiling, not a protection; the measured suppression is coercive power. The engine captures this divergence by computing directionality per seat and deriving types from the seats' structural relationships to the constraint, not from the claim. This is deliberate—the claim/metrics gap is the point. A claim of 'tangled_rope' (genuine coordination with asymmetric extraction) survives if credentialed institutions can show the gatekeeping is necessary for knowledge quality; it fails if non-credentialed knowledge systems achieve equivalent outcomes without the gatekeeping. This is not a matter of opinion—it is a testable structural question about whether the coordination function and the extraction function are separable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim position and exit options. Credentialed institutions have low d (full beneficiary end): they set the rules, collect the rents (funding, prestige, authority), and have arbitrage-grade exit (they can always hire away talent from rivals, change the rules, adapt to new knowledge validation methods). Credentialed experts have moderate-to-low d (beneficiary-leaning, with some extraction from credential inflation): they benefit from the authority presumption but also pay for maintaining it (publish-or-perish, peer review labor, credential credentialization). Non-credentialed practitioners have high d (full target end) and identity-locked exit: the constraint defines them as illegitimate and locks them in by making legitimacy fused with credentialed identity. Experiential knowledge communities have high d (target) but constrained exit (they can organize, mount class-level resistance, but structural exit from the constraint itself is not available—the constraint is what defines them as 'experiential'). Excluded epistemologies have extreme high d and trapped exit: they have no way out without wholesale adoption of credentialed norms. Policy makers have low-to-moderate d: they benefit from the truth-proxy function but are also constrained by it (they must defer to experts even when experts disagree). This directionality structure—beneficiaries at both structural-power ends, victims clustered at powerless and identity-locked positions—is what makes this a snare/tangled-rope candidate rather than a rope. A pure rope would have symmetric exit options and diffuse cost; this has asymmetric exit locked into identity and power structures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unvalidated claims capturing authority through rhetoric, methodology being hijacked by patronage) was substantially solved by the mid-20th century. Peer review works: obvious junk is filtered; systematic error is usually caught; replication-failure reduces survival of false claims. But the constraint persists with rising extractiveness (0.68) and rising theater (0.42) because the benefiting parties (academic institutions, journal publishers, credentialed gatekeepers) have now accumulated sufficient power that the constraint maintains itself through institutional inertia and rent-seeking, independent of the coordination need. The evidence: (1) Credential inflation accelerates despite no proportional improvement in knowledge quality; (2) Peer review has become bottleneck rather than filter—review speed slows while publication costs rise; (3) Non-credentialed but systematically validated knowledge (open-source software, citizen science, farmer seed networks) achieve reliable outcomes comparable to credentialed research, suggesting the gatekeeping is not necessary for quality; (4) Alternative validation methods (replication markets, prediction markets, post-publication review) emerge outside the credentialed system and work; (5) Policy decisions increasingly require credentialed evidence even when non-credentialed evidence is equally informative, suggesting the constraint serves legitimacy-theater more than knowledge-quality. This is a mandatrophy case: the founding problem no longer exists at scale, the constraint persists because the beneficiaries benefit from maintaining it, and the cost of fixing it (dismantling credentialing as a sole legitimacy criterion) exceeds the benefit to any single party except the excluded communities. Mandatrophy resolution requires collective action among the excluded and the heretical to dismantle the constraint's enforcement machinery—a task now underway through open science, Indigenous data sovereignty, and methodological pluralism movements. The claim survives the analysis (tangled_rope with real coordination function + real extraction), but the extraction component is now more substantial than the claim's authors would admit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_gatekeeping_separability,
    'Is the coordination function of shared epistemological standards structurally inseparable from the gatekeeping function of credentialing, or can coordination be achieved through alternative validation mechanisms without centralized gatekeeping?',
    'Natural experiments from jurisdictions or domains that have pluralized validation (open-source software validation through use + reputation, decentralized clinical trials, Indigenous data sovereignty initiatives): do knowledge quality and cumulative capability remain intact without credentialed gatekeeping? Do alternative validation mechanisms solve the original founding problem (preventing charlatanism and false consensus)?',
    'If coordination and gatekeeping are separable, the constraint reclassifies as snare (gatekeeping is pure extraction with coordination as cover story). If inseparable, the constraint remains tangled_rope (genuine coordination with asymmetric extraction cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_gatekeeping_separability, empirical, 'Whether credentialing is a necessary component of knowledge coordination or a separable extraction mechanism.').

omega_variable(
    credential_inflation_endogenous_to_gatekeeping,
    'Does credential inflation arise as an inevitable side effect of credentialed gatekeeping (beneficiaries raising barriers to entry to maintain scarcity of the credential), or is it driven by external factors (growing knowledge complexity, increasing policy stakes)?',
    'Historical comparison: domains where gatekeeping power is low (e.g., physics after the Internet enabled preprint distribution) vs. high (e.g., medicine before Internet open-access); do credentials inflate at different rates? Controlled policy experiment: remove credentialing requirement from a domain and measure whether credential demands decrease or continue inflating.',
    'If credential inflation is endogenous (gatekeepers drive it), the extraction is intentional and the constraint is more snare-like (extractive through rent-seeking). If exogenous, the extraction is a side effect and the tangled_rope characterization is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_inflation_endogenous_to_gatekeeping, empirical, 'Whether credential inflation is a feature or a side-effect of the gatekeeping mechanism.').

omega_variable(
    reading_foreclosure_and_epistemic_pluralism,
    'Does the credentialed-expertise reading logically foreclose the experiential-pluralism reading within a single epistemic framework, or do the readings represent genuinely incommensurable frameworks between which choice is a matter of values, not logic?',
    'Philosophical analysis: can a coherent epistemic framework (one set of axioms about what counts as knowing) hold both (1) methodological rigor is the primary legitimacy criterion AND (2) lived experience and community validation are equally legitimate? If yes, the readings coexist; if no, one forecloses the other.',
    'If foreclosure is real, this reading''s relationship to experiential-pluralism is ''forecloses'' (not ''coexists_with''); the kernel represents a zero-sum choice, not a pluralistic ecosystem. If the readings are incommensurable but non-foreclosing, they coexist in different institutions and jurisdictions, and neither can claim to have ruled the other out in principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_and_epistemic_pluralism, conceptual, 'Whether credentialed-expertise and experiential-pluralism readings can coexist in a single coherent framework or represent incompatible epistemic commitments.').

omega_variable(
    identity_lock_mechanism_internalized_vs_structural,
    'Is the identity-lock experienced by non-credentialed practitioners (the internalized belief that their knowledge is inferior unless credentialed) structurally imposed by the constraint''s enforcement machinery (exclusion from funding, policy, publication), or internalized through socialization and culture?',
    'Post-exit trajectory: when non-credentialed practitioners gain credentials or move to jurisdictions/communities where credentialing is not required, does the identity-lock persist (internalized) or dissolve (structural)? Do practitioners report their sense of knowledge legitimacy as a function of credential status or of community validation?',
    'If internalized, the constraint''s effective suppression is higher than the measured suppression suggests—the target carries the suppression with them after structural exit. If structural, the suppression is reversible and post-exit reverts to pre-constraint levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalized_vs_structural, empirical, 'Whether identity-lock is internalized through epistemic socialization or structural via institutional gatekeeping.').

omega_variable(
    kernel_reading_selection_dependence,
    'Is the credentialed-expertise reading a legitimate interpretation of the kernel (the contest over what makes knowledge legitimate is genuinely three-way, and this reading is one coherent answer), or is it the reading authored from the position of power (institutions that benefit from it define the reading to justify their position)?',
    'Genealogical analysis: did credentialed institutions adopt the credentialed-expertise framing to legitimize their pre-existing gatekeeping power, or did they develop credentialing as a tool to operationalize a genuinely prior commitment to methodological rigor? Does the reading represent an independent epistemic value or post-hoc rationalization of institutional power?',
    'If the reading is post-hoc rationalization, it should not be treated as an equal-standing reading but as a false-summit constraint (coordinate appearance with extraction beneath). If it is a genuinely independent epistemic commitment, it stands as one reading among three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_dependence, conceptual, 'Whether the credentialed-expertise reading is an authentic epistemic position or a rationalization of institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__credentialed_expertise_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(legi_tr_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(legi_tr_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(legi_tr_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(legi_tr_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(legi_tr_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(legi_be_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(legi_be_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(legi_be_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(legi_be_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(legi_be_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(legi_be_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t8, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(legi_su_t16, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(legi_su_t24, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(legi_su_t32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(legi_su_t40, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(legi_su_t50, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression_requirement, 50, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(legi_grid_01, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(legi_grid_02, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(class), 50, 0.71).
narrative_ontology:measurement(legi_grid_03, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(individual), 0, 0.58).
narrative_ontology:measurement(legi_grid_04, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(individual), 50, 0.68).
narrative_ontology:measurement(legi_grid_05, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(legi_grid_06, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(organizational), 50, 0.77).
narrative_ontology:measurement(legi_grid_07, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(structural), 0, 0.68).
narrative_ontology:measurement(legi_grid_08, legitimate_knowledge_boundary__credentialed_expertise_reading, accessibility_collapse(structural), 50, 0.73).
narrative_ontology:measurement(legi_grid_09, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_10, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(class), 50, 0.62).
narrative_ontology:measurement(legi_grid_11, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(legi_grid_12, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(individual), 50, 0.58).
narrative_ontology:measurement(legi_grid_13, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(organizational), 0, 0.38).
narrative_ontology:measurement(legi_grid_14, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(organizational), 50, 0.52).
narrative_ontology:measurement(legi_grid_15, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(structural), 0, 0.42).
narrative_ontology:measurement(legi_grid_16, legitimate_knowledge_boundary__credentialed_expertise_reading, resistance(structural), 50, 0.48).
narrative_ontology:measurement(legi_grid_17, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(legi_grid_18, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(class), 50, 0.65).
narrative_ontology:measurement(legi_grid_19, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(legi_grid_20, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(individual), 50, 0.62).
narrative_ontology:measurement(legi_grid_21, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(organizational), 0, 0.7).
narrative_ontology:measurement(legi_grid_22, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(organizational), 50, 0.75).
narrative_ontology:measurement(legi_grid_23, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(legi_grid_24, legitimate_knowledge_boundary__credentialed_expertise_reading, stakes_inflation(structural), 50, 0.68).
narrative_ontology:measurement(legi_grid_25, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(legi_grid_26, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(class), 50, 0.71).
narrative_ontology:measurement(legi_grid_27, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement(legi_grid_28, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(individual), 50, 0.68).
narrative_ontology:measurement(legi_grid_29, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(organizational), 0, 0.65).
narrative_ontology:measurement(legi_grid_30, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(organizational), 50, 0.75).
narrative_ontology:measurement(legi_grid_31, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(structural), 0, 0.6).
narrative_ontology:measurement(legi_grid_32, legitimate_knowledge_boundary__credentialed_expertise_reading, suppression(structural), 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__credentialed_expertise_reading, information_standard).
narrative_ontology:boltzmann_floor_override(legitimate_knowledge_boundary__credentialed_expertise_reading, 0.12).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__experiential_pluralism_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, legitimate_knowledge_boundary__hybrid_coproduction_reading).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, academic_publishing_fee_barrier).
narrative_ontology:affects_constraint(legitimate_knowledge_boundary__credentialed_expertise_reading, credential_inflation_premium).

% DUAL FORMULATION NOTE:
% This constraint story is part of a three-story family decomposing the contested kernel 'legitimate knowledge boundary.' The family has a non-linearh structure: the credentialed-expertise reading (this story) influences both the experiential-pluralism and hybrid-coproduction readings by setting the institutional constraints they must work within or against. The hybrid-coproduction reading influences back: successful integration of methodological and experiential validation changes the landscape both readings operate in. The experiential-pluralism reading coexists with this one; neither rules the other out, but they occupy different institutional spaces and make incompatible claims. Each story has its own epsilon, its own beneficiary/victim structure, and its own classification; the engine computes them independently. The network relationships enable contamination analysis: if the credentialed-expertise reading degrades (loses suppression capacity, encounters organized resistance), how do the sibling readings gain space?

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_knowledge_boundary__credentialed_expertise_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
