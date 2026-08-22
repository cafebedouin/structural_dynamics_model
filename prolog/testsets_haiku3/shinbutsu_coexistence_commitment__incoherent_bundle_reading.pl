% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo Incoherent Bundle: Institutional Maintenance through Deliberate Ambiguity
 *   domain: religious/philosophical/political
 *
 * SUMMARY:
 *   Shinbutsu-shugo (Buddhism-Shinto coexistence) persisted from the 9th
 *   century until Meiji bunri (separation) in 1868 as an institutional
 *   arrangement, not as a coherent theological framework. This reading holds
 *   that the system was sustained by deliberate institutional maintenance of
 *   doctrinal ambiguity: Buddhist temples administered kami shrines; kami
 *   were invoked as manifestations of Buddhas (honji suijaku) without
 *   resolving what this meant ontologically; local communities paid dues to
 *   both networks; and the imperial state enforced the incoherence by
 *   suppressing attempts to resolve it. The system collapsed not because
 *   logical pressure finally broke through, but because Meiji
 *   reformers—standing outside both institutional networks—declared that kami
 *   and Buddhism would henceforth be separated, revealing the incoherence as
 *   incoherence only through the act of declaration. This reading differs
 *   from the syncretic-fusion reading (which posits genuine ontological
 *   unification) and the domain-partition reading (which posits separate
 *   cosmological domains). This reading claims there was no stable
 *   metaphysical solution underlying the system—only institutional power
 *   maintaining useful ambiguity.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: agenda-setter and beneficiary. Operated both Buddhist temples and kami shrines under unified control, collecting revenues regardless of doctrinal framework. Authority derived from institutional position, not theological coherence.
 *   - Shinto shrine networks: agenda-setter and beneficiary. Maintained local autonomy by accepting Buddhist theoretical frameworks while operating independently in practice. Claimed both Buddhist legitimacy and indigenous authenticity without resolving contradiction.
 *   - Imperial administrative apparatus: agenda-setter. Maintained shinbutsu-shugo to govern competing religious institutions without declaring metaphysical preference. Incoherence was operational convenience, not a problem requiring solution.
 *   - Lay practitioners: payer and primary victim. Navigated incoherence through compartmentalization (kami shrines for immediate concerns, temples for death and salvation) without demanding theoretical unity. Bore the cost of dual obligation.
 *   - Doctrinal philosophers: payer and partially excluded. Attempted systematic reconciliation under institutional pressure. Publishing theories that resolved incoherence too decisively threatened both institutional networks' autonomy.
 *   - Meiji reformers: observer seat. Implemented shinbutsu bunri as policy choice, revealing the system had been maintained by institutional power, not by having solved a genuine theoretical problem.
 *   - Theological unity advocates: excluded. Marginalized by both institutional networks because unified doctrine would eliminate the autonomy both enjoyed under ambiguity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.72).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo Incoherent Bundle: Institutional Maintenance through Deliberate Ambiguity").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious/philosophical/political").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '93b873ff-5d4c-455c-9bfc-284a57b6e424').
narrative_ontology:cs_kernel_codification('93b873ff-5d4c-455c-9bfc-284a57b6e424', distributed).
narrative_ontology:cs_authority_grounding('93b873ff-5d4c-455c-9bfc-284a57b6e424', extraction).
narrative_ontology:cs_interpretation_layer_present('93b873ff-5d4c-455c-9bfc-284a57b6e424').
narrative_ontology:cs_reading_relation('93b873ff-5d4c-455c-9bfc-284a57b6e424', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('93b873ff-5d4c-455c-9bfc-284a57b6e424', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('93b873ff-5d4c-455c-9bfc-284a57b6e424', foundational, ambiguity_as_institutional_maintenance).
narrative_ontology:cs_axiom_status(ambiguity_as_institutional_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('93b873ff-5d4c-455c-9bfc-284a57b6e424', ambiguity_as_institutional_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('93b873ff-5d4c-455c-9bfc-284a57b6e424', foundational, coherence_frameworks_as_cover_stories).
narrative_ontology:cs_axiom_status(coherence_frameworks_as_cover_stories, holdable).
narrative_ontology:cs_axiom_grounding('93b873ff-5d4c-455c-9bfc-284a57b6e424', coherence_frameworks_as_cover_stories, empirically_contingent).
narrative_ontology:cs_reference_frame('93b873ff-5d4c-455c-9bfc-284a57b6e424', institutional_coexistence_through_ambiguity_maintenance).
narrative_ontology:cs_drift_state('93b873ff-5d4c-455c-9bfc-284a57b6e424', late_edo_to_meiji_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('93b873ff-5d4c-455c-9bfc-284a57b6e424', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrine_networks).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, imperial_administrative_apparatus).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_navigating_contradictions).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, doctrinal_philosophers_seeking_coherence).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities_dual_obligation_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Buddhist temples and their networks administer the ambiguous coexistence system, performing both Buddhist rituals and kami rituals under unified organizational control. They collect revenues from parishioners regardless of which ritual framework is invoked. The doctrinal incoherence allows them to maintain institutional autonomy without settling theological questions that would constrain their authority structure or require subordinating kami worship to Buddhist doctrine. Their power derives from institutional position, centuries of accumulated property and prestige, and control of both ritual and administrative functions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutional_hierarchy, beneficiary).

% Shinto shrines maintained ritual autonomy and local authority by accepting Buddhist theoretical frameworks (honji suijaku, canonical integration) while operating independently in actual practice. The ambiguity allowed them to claim both Buddhist legitimacy (enhancing prestige and stability) and indigenous spiritual authenticity (maintaining local identity and community authority) simultaneously. They collected offerings and maintained community-level power without submitting to doctrinal resolution that would subordinate kami to Buddhist ontology or require explicit separation that would reduce their institutional reach.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrine_networks, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinto_shrine_networks, beneficiary).

% The imperial state maintained shinbutsu-shugo as an administrative convenience that allowed the state to govern both Buddhist and kami institutional networks without declaring a metaphysical preference. Avoiding doctrinal clarity prevented conflicts that would fragment the state's ability to tax and control the religious institutions. The incoherence was not a problem requiring solution but a feature enabling state control over competing religious hierarchies. The state actively enforced the maintenance of ambiguity by suppressing attempts to resolve it through policy and by withholding support from institutional reformers who sought coherence.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, imperial_administrative_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Ordinary practitioners navigated the incoherent bundle by compartmentalizing: visiting kami shrines for immediate practical concerns (harvests, health, purification, life transitions) and Buddhist temples for death rituals, merit-making, and cosmological salvation. They paid dues and offerings to both institutional networks and were obligated to participate in both ritual calendars. The fundamental contradiction between kami as autonomous spiritual beings and kami as local manifestations of Buddhist truths was never resolved for them; they absorbed the incoherence by treating rituals as pragmatically separate rather than demanding ontological unity. This compartmentalization was presented as natural rather than as a burden imposed by institutional interests.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_practitioners_navigating_contradictions, payer,
    powerless, biographical, constrained, local).

% Buddhist and Shinto theological scholars who attempted systematic reconciliation of the two systems (through refined honji suijaku theories, domain-partition frameworks, or genuine synthetic ontologies) operated under constant institutional pressure and professional risk. Publishing theories that resolved the incoherence too decisively threatened both institutional networks' operational autonomy and their ability to control ritual and doctrinal authority. Scholars remained embedded within their respective traditions; attempting to construct unified philosophy across the traditions risked professional marginalization and loss of institutional support. Their intellectual work was constrained by the institutional need to preserve productive ambiguity.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, doctrinal_philosophers_seeking_coherence, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_coexistence_commitment__incoherent_bundle_reading, doctrinal_philosophers_seeking_coherence, excluded).

% Villages and towns were embedded in both institutional networks simultaneously, obligated to maintain both kami shrines and Buddhist temples, fund both ritual calendars, support both priesthoods, and participate in both ritual cycles. The dual obligation was presented as natural, necessary, and cosmically appropriate but was never justified through a stable, coherent doctrine. It persisted through inertia, custom, institutional enforcement, and the structural impossibility of exit without severing community identity and access to spiritual services. Communities bore the accumulated economic and organizational cost of maintaining two parallel institutional systems whose relationship was never resolved.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, local_communities_dual_obligation_burden, payer,
    powerless, generational, trapped, local).

% Meiji state actors and modernizing intellectuals implemented shinbutsu bunri (separation of kami and Buddhist spheres) as a deliberate policy choice, not as the logical discovery of hidden coherence or the necessary working-out of dormant contradictions. Their perspective revealed that shinbutsu-shugo had been sustained by institutional power and political convenience, not by having genuinely solved a metaphysical problem. Reformers could see the incoherence as incoherence, and declare that the system must end, precisely because they stood outside both institutional networks and could impose change unilaterally. They frame separation as a return to truth; the incoherent_bundle_reading frames it as a revelation of power.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_reformers, observer,
    institutional, biographical, analytical, national).

% Scholars and priests who advocated for genuine ontological synthesis (actual fusion of kami and Buddhist metaphysics rather than convenient coexistence or domain partition) were marginalized and excluded from institutional authority by both Buddhist and Shinto hierarchies. Their exclusion occurred because a unified coherent doctrine would require one tradition to subordinate the other or both to abandon core identity claims, eliminating the institutional autonomy both enjoyed under the maintained ambiguity. Their voices were systematically excluded because institutional interest lay in preservation of incoherence, not in resolution toward truth. They remained alive in academic and sectarian margins but lacked the institutional power to reshape the system.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theological_unity_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevented institutional conflict between two major religious systems (kami worship and Buddhism) by creating a unified administrative framework where temples controlled both Buddhist and kami ritual spaces, allowing both networks to operate without declaring doctrinal preference or institutional subordination. Solved the problem of dual religious obligation for communities without requiring them to choose between traditions.
% TRANSFER_FUNCTION: Moved authority, revenue, and doctrinal freedom from lay practitioners and local communities to institutional hierarchies (Buddhist temples, Shinto shrine networks, imperial administration). Practitioners paid dues to both institutional networks and accepted theological incoherence in exchange for access to both ritual frameworks. The transfer was also of intellectual work: doctrinal philosophers and theological advocates bore the cost of attempting coherence, which institutional networks suppressed to preserve their operational freedom.
% ABSENT_VOICES: Theological unity advocates who sought genuine ontological synthesis (neither honji suijaku nor domain partition, but actual unified doctrine) were excluded by both institutional networks because coherent doctrine would eliminate institutional autonomy. Reformers outside both religious systems (Western-educated critics, Meiji state actors not beholden to either network) were not in the conversation until they became powerful enough to declare the system must end. Their exclusion was structural: insiders could not raise the question without threatening institutional interests.
% DISAPPEARANCE_RATIONALE: If shinbutsu-shugo had not existed, medieval and early modern Japan would have developed either a dominant-religion structure (Buddhism subordinating kami worship, or kami worship marginalizing Buddhism) or explicit institutional separation much earlier. The dual system's persistence shaped institutional development, property holdings, ritual calendars, community obligations, and state administrative capacity for eight centuries. Its disappearance reorganized all of these; the Meiji bunri was a massive institutional restructuring precisely because the coexistence arrangement had become deeply embedded. Communities, temples, and shrines all reorganized their operations in response to separation.
% FOUNDING_PROBLEM: Early medieval Japan faced the problem of integrating an imported universal religion (Buddhism, arriving in the 6th century) with an indigenous religious system (kami worship) without producing institutional collapse or forcing communities to abandon ancestral practices. By the 9th century, this had crystallized into a coordination problem: two institutional networks requiring resources, both embedded in local communities, neither able to be eliminated without social disruption.
% FOUNDING_PROBLEM_CORROBORATION: The imperial state (Meiji reformers) attests the founding problem was no longer live—it could impose bunri without institutional collapse. Buddhist and Shinto institutions initially resisted separation, suggesting they benefited from coexistence but could adapt to separation, indicating the founding coordination problem had been solved and was no longer driving the system. Scholars outside both networks (Western-educated intellectuals, modernizing administrators) attested that the incoherence had become a liability rather than a solution. The founding problem is attested as dead by all non-beneficiary parties; only the institutional beneficiaries (temples, shrines) resisted its dissolution because they were profiting from ambiguity-maintenance.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at interval end is 0.68, high because the institutional networks (Buddhist and Shinto) extracted authority, revenue, and doctrinal freedom from lay practitioners and communities without providing a stable framework in return. Suppression is 0.72, tracking with active enforcement: the imperial state and institutional hierarchies actively suppressed attempts to resolve the incoherence (doctrinal philosophers faced marginalization; theological unity advocates were excluded). Theater ratio rises from 0.35 to 0.58 across the interval, indicating that as external pressures mounted (Western critique, Meiji rationalization discourse, scholarly attempts at coherence), the system increasingly relied on performative maintenance of ambiguity rather than on the ambiguity's practical utility. Accessibility collapse is 0.41 (relatively low): alternatives existed (pure Buddhism, pure Shinto, coherent syncretic theories) but institutional power and inertia suppressed them at the system level, though not entirely at the local level where communities could emphasize one framework over the other. Resistance is 0.54: genuine resistance came from doctrinal philosophers and theological advocates who recognized the incoherence, but this resistance was suppressed institutionally and remained marginal. The temporal series shows extractiveness and suppression rising together as the system's contradictions became harder to maintain without explicit enforcement; theater rising indicates the shift from ambiguity-as-convenience to ambiguity-as-performance, a diagnostic piton-adjacent dynamic. Measurement grid aligned: all metrics authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   Institutional beneficiaries (Buddhist hierarchy, Shinto networks, imperial state) experienced shinbutsu-shugo as a stable, workable coordination system: they extracted revenue and authority without the constraint of resolving theological contradiction, and could move between frameworks as institutional convenience dictated. Victims (lay practitioners, local communities, doctrinal philosophers) experienced it as an incoherent burden: they were obligated to maintain dual practice, fund dual institutions, and accept contradiction without resolution. The divergence reflects asymmetric power: those who maintained ambiguity did so from a position of institutional control; those embedded in the system could not exit without severing identity and community ties. The engine should compute this divergence from the structural data: beneficiaries carry low d (extraction-dampening), victims carry high d (extraction-amplifying), and the identity_locked exit condition on doctrinal philosophers' professional identities feeds a tight coupling to the constraint. From the beneficiary institutional seats, the constraint computed type should be coordination-adjacent or rope-like; from the victim seats, it should compute as tangled_rope or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutional hierarchy: beneficiary role, institutional power, constrained exit (cannot dissolve without disavowing centuries of accumulated authority and property). D derives near 0.2–0.3 (beneficiary end): they profit from maintenance and their exit is not authentically available. Shinto shrine networks: similar beneficiary structure, slightly more mobile exit (could theoretically break with Buddhist administrative systems) but identity-locked to autonomous local authority sustained by ambiguity. D approximately 0.25–0.35. Imperial state: agenda-setter role, institutional power, mobile exit (chose to exit via Meiji bunri). D approximately 0.3–0.4: they set and enforce the arrangement but are not trapped by it. Lay practitioners: payer role, powerless position, constrained exit (embedded in local communities where both institutions are obligatory). D approximately 0.75–0.85: they bear the cost with minimal exit options. Doctrinal philosophers: payer role, moderate power (intellectual authority within their traditions), identity-locked exit (professional identity dependent on remaining within Buddhist or Shinto schools). D approximately 0.65–0.75: they pay through marginalization and intellectual constraint while being trapped by professional identity. Local communities: payer role, powerless, trapped exit (obligated to both shrine and temple, geographically embedded). D approximately 0.80–0.90: maximum target positioning. No override needed; structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine: early medieval Japan needed a framework to coordinate two initially distinct religious systems (kami worship and imported Buddhism) without producing violent institutional conflict. Honji suijaku and informal coexistence solved that problem for roughly eight centuries. By the late Edo period and into Meiji, the founding problem had substantially atrophied: the institutions had consolidated into stable hierarchies, the immediate risk of conflict had diminished, and the intellectual cost of maintaining the incoherence had risen (Western philosophical critique, Meiji rationalization discourse, accumulated scholarly attempts at resolution). Yet the arrangement persisted because beneficiary institutions (Buddhist temples, Shinto shrines, imperial administration) had no incentive to dismantle it unilaterally. The Meiji bunri was not a logical necessity emerging from accumulated theoretical pressure; it was a policy decision by a reforming state that could declare the incoherence must end and impose separation. This is the signature of mandatrophy: a coordination function that addressed a real problem becomes an extraction mechanism sustained past its utility by institutional inertia. Classification as tangled_rope (not snare) reflects the fact that a genuine coordination problem was solved by the system—the coexistence framework prevented institutional collapse—but the persistence of the framework past its functional need, sustained through ambiguity-maintenance and institutional power, makes it extractive. The theater_ratio's rise to 0.58 signals the system increasingly relying on performative maintenance (ritual affirmation of coexistence, scholarly rehearsal of reconciliation theories, administrative enforcement of dual obligation) rather than on practical utility. The constraint did not vanish when mandatrophy was exposed; Meiji reformers had to impose it through political decree. This reflects both the institutional entrenchment of the beneficiaries and the powerlessness of the victims to force change unilaterally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_power_vs_doctrine,
    'Was shinbutsu-shugo maintained primarily by institutional power and convenience, or did it rest on genuine theological consensus that practitioners and scholars actually believed?',
    'Examination of private correspondence, sectarian documents, and heterodox theological attempts (which proliferated) to determine whether the incoherence was consciously maintained or genuinely unrecognized. Analysis of institutional responses to coherence-seeking theories: suppression indicates maintenance; adoption indicates consensus.',
    'If maintained by power, the constraint is extractive coordination (tangled_rope) sustained past its functional need. If resting on genuine theological consensus, it might be a stable rope or even a mountain-adjacent feature of medieval Japanese thought.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_power_vs_doctrine, empirical, 'Whether incoherence was consciously maintained or genuinely unrecognized by participants.').

omega_variable(
    ambiguity_functional_necessity,
    'Was the institutional coexistence dependent on doctrinal ambiguity, or was the ambiguity merely a tolerated side-effect of independent institutional development?',
    'Counterfactual analysis: could Buddhist temples and Shinto shrines have coexisted under an explicit coherence theory (honji suijaku, domain partition, or fusion) without losing institutional autonomy? Historical examination of moments where coherence was attempted.',
    'If ambiguity was functionally necessary, it was a feature of the coordination mechanism. If ambiguity was incidental, the constraint''s persistence despite incoherence indicates pure institutional entrenchment (piton-like dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_functional_necessity, conceptual, 'Whether doctrinal ambiguity was structurally necessary or contingently maintained.').

omega_variable(
    lay_practitioner_awareness,
    'To what degree did ordinary practitioners consciously experience the incoherence as incoherence, versus compartmentalizing without recognizing contradiction?',
    'Analysis of folk narratives, popular religious texts, and evidence of local theological discussion. Examination of whether dual practice was presented as a paradox or as pragmatic complementarity.',
    'If practitioners consciously held the contradiction, suppression operated primarily through institutional power and narrative reframing. If the incoherence was invisible at the lay level, suppression operated through compartmentalization and inertia rather than active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lay_practitioner_awareness, empirical, 'Whether incoherence was consciously experienced or institutionally hidden from practitioners.').

omega_variable(
    reading_disagreement_location,
    'Where does the fundamental disagreement lie between this reading and the syncretic_fusion_reading and domain_partition_reading?',
    'Examination of honji suijaku theory and domain-partition frameworks to determine whether they represent genuine theoretical solutions attempted and adopted, or post-hoc rationalizations that never resolved the underlying incoherence.',
    'If honji suijaku and domain-partition theories were real solutions that practitioners believed in, those readings are correct. If they were intellectual cover stories for institutional autonomy, this reading (incoherent bundle) is structurally accurate. The disagreement is located in the ontological status of the reconciliation frameworks themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_disagreement_location, conceptual, 'The core structural disagreement between competing readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(shin_tr_t0, observed).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(shin_tr_t20, observed).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement_basis(shin_tr_t40, observed).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 60, 0.53).
narrative_ontology:measurement_basis(shin_tr_t60, observed).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 80, 0.56).
narrative_ontology:measurement_basis(shin_tr_t80, observed).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 100, 0.58).
narrative_ontology:measurement_basis(shin_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(shin_be_t0, observed).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(shin_be_t20, observed).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(shin_be_t40, observed).
narrative_ontology:measurement(shin_be_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(shin_be_t60, observed).
narrative_ontology:measurement(shin_be_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement_basis(shin_be_t80, observed).
narrative_ontology:measurement(shin_be_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 100, 0.68).
narrative_ontology:measurement_basis(shin_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(shin_su_t0, observed).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(shin_su_t20, observed).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement_basis(shin_su_t40, observed).
narrative_ontology:measurement(shin_su_t60, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(shin_su_t60, observed).
narrative_ontology:measurement(shin_su_t80, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement_basis(shin_su_t80, observed).
narrative_ontology:measurement(shin_su_t100, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 100, 0.72).
narrative_ontology:measurement_basis(shin_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__incoherent_bundle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.12).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__syncretic_fusion_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_bunri_policy_implementation).

% DUAL FORMULATION NOTE:
% This constraint represents one reading of the shinbutsu_coexistence_commitment kernel. Three competing readings exist: syncretic_fusion_reading (genuine ontological unification through honji suijaku), domain_partition_reading (separate cosmological domains without unification), and this incoherent_bundle_reading (no stable resolution, system maintained by institutional power and deliberate ambiguity). The three readings share the same historical arrangement but make fundamentally different claims about its nature. The divergence is located in the ontological status of the reconciliation frameworks (honji suijaku, domain partition) and the causal role of institutional power in sustaining the system. This reading influences both sibling readings by claiming that their respective theoretical frameworks (syncretic unity, domain partition) are post-hoc rationalizations rather than the actual logic of the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_coexistence_commitment__incoherent_bundle_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
