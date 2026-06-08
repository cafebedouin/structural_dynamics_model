% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity_flat_control, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_continuity_flat_control
 *   human_readable: Sacrificial Law Obligation Continuity Despite Physical Impossibility
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   The constraint examined here is the obligation to maintain sacrificial
 *   law as a binding divine commandment despite the physical impossibility of
 *   its performance — a paradox that has structured Jewish legal tradition
 *   for nearly two millennia. When the Second Temple was destroyed in 70 CE,
 *   the material conditions for sacrifice (Temple, altar, priesthood
 *   operating in the prescribed location) were permanently foreclosed. Yet
 *   the laws mandating sacrifice were never formally revised or rescinded.
 *   Instead, the tradition institutionalized a paradox: the law remains
 *   binding and eternal, but compliance is redefined through substitution
 *   (prayer as sacrifice, study as Temple service, textual recitation as
 *   ritual performance). This constraint exhibits tangled rope structure from
 *   most perspectives — genuine coordination function (maintaining communal
 *   identity and textual continuity across diaspora) combined with asymmetric
 *   extraction (the lay practitioner is bound to obey an unperformable law,
 *   while the interpretive authority class benefits from ongoing necessity to
 *   adjudicate impossible compliance). The theater ratio shows dramatic
 *   increase over the interval: early post-Temple rabbinic authorities
 *   explicitly acknowledged the substitution as temporary (awaiting
 *   restoration); by the medieval period (t=500), substitute rituals had
 *   become institutionalized as permanent workarounds; by the modern period
 *   (t=1500), the system's performative character dominates — the obligation
 *   is maintained in legal codes and commentaries, performed through
 *   substitutes everyone knows are not true sacrifice, and defended through
 *   elaborate exegesis that acknowledges replacement while denying
 *   incompleteness. The lay practitioner experiences this as identity lock:
 *   the obligation persists as binding, non-compliance is not a live option
 *   (it means abandoning religious identity and community), and the
 *   impossibility of true performance creates permanent cognitive bind.
 *
 * KEY AGENTS:
 *   - Lay Practitioner Community: Primary victim (powerless/identity_locked) — bears binding obligation to perform impossible law; cannot exit without abandoning religious identity and community membership
 *   - Interpretive Authority Structure (Rabbinic Class): Primary beneficiary (institutional/arbitrage) — maintains institutional necessity through ongoing adjudication of what binding law requires; collects authority rent from being sole arbiter of compliance in impossibility
 *   - Diaspora Community: Secondary agent (moderate/constrained) — genuine coordination function (maintaining communal identity across exile) but also bears asymmetric extraction (obligation persists while means are foreclosed)
 *   - Reform/Critical Movements: Organized agents (organized/mobile) — propose revising the binding status of sacrificial law; see constraint as temporary scaffold with available sunset path via doctrinal revision
 *   - Orthodox Institutional Consensus: Institutional actor (institutional/constrained) — maintains paradox through institutional inertia and textual reverence; theater is high (everyone knows substitutes are not true sacrifice) but exit is constrained by career cost of revision
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the institutional paradox as inherent divine law; perceives mountain when the structure is actually tangled rope maintained by beneficiary extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity_flat_control, 0.58).
domain_priors:suppression_score(sacrifice_obligation_continuity_flat_control, 0.62).
domain_priors:theater_ratio(sacrifice_obligation_continuity_flat_control, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity_flat_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity_flat_control, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity_flat_control, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity_flat_control, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity_flat_control, "Sacrificial Law Obligation Continuity Despite Physical Impossibility").
narrative_ontology:topic_domain(sacrifice_obligation_continuity_flat_control, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity_flat_control, '5d74bc1b-63e5-4dd6-9c7c-9069f2f2a9a9').
narrative_ontology:cs_kernel_codification('5d74bc1b-63e5-4dd6-9c7c-9069f2f2a9a9', fixed_text).
narrative_ontology:cs_authority_grounding('5d74bc1b-63e5-4dd6-9c7c-9069f2f2a9a9', extraction).
narrative_ontology:cs_interpretation_layer_present('5d74bc1b-63e5-4dd6-9c7c-9069f2f2a9a9').
narrative_ontology:cs_created_at('5d74bc1b-63e5-4dd6-9c7c-9069f2f2a9a9', '2026-02-26T14:32:00Z').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(sacrifice_obligation_continuity_flat_control, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity_flat_control, interpretive_authority_structure).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity_flat_control, priestly_jurisprudence_class).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity_flat_control, lay_practitioner_compliance_obligation).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity_flat_control, textual_coherence_epistemic_commons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity_flat_control, interpretive_authority_class).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity_flat_control, diaspora_community).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity_flat_control, lay_practitioner_cohort).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity_flat_control, diaspora_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by inherited tradition and community membership to recognize sacrificial law as binding divine commandment. Cannot perform the law (no Temple, no altar, no priesthood in correct location). Experiences permanent cognitive dissonance: the law is binding and eternal, yet unperformable. Exit from the obligation means abandoning religious identity and losing community belonging. Participates in substitute rituals (prayer, study, textual recitation) that are understood to not constitute true sacrifice but to maintain nominal compliance with the binding obligation. The impossibility of true performance is permanent — not temporary or awaiting restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity_flat_control, lay_practitioner_cohort, payer,
    powerless, biographical, identity_locked, regional).

% Rabbinic scholars and institutional leaders who control the interpretation of what binding law requires when performance is impossible. Benefit from ongoing institutional necessity to adjudicate the paradox. Maintain the law's binding status while redefining compliance through substitution. Collect interpretive authority rent: without them, the paradox is unresolvable (how can lay practitioners obey an unperformable law?). With them, the system functions through elaborate exegesis and workaround mechanisms. Their career advancement, institutional status, and intellectual authority depend on being the necessary translators between binding law and impossible performance.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity_flat_control, interpretive_authority_class, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity_flat_control, interpretive_authority_class, agenda_setter).

% Jewish communities scattered across geographic diaspora and centuries of displacement. The sacrifice law obligation (now impossible to perform literally) becomes the vehicle for maintaining communal identity, textual continuity, and religious practice across separation and exile. The coordination function is genuine — without the continuity mechanism, diaspora communities might lose connection to the founding tradition. But they also bear extraction: they must maintain the obligation as binding even though it cannot be fulfilled; their religious practice is constrained by paradoxical legal structures; their intellectual freedom is limited by institutional insistence on the law's binding status. They have some agency (can interpret the law, participate in shaping practice) but significant constraint (cannot revise the law's binding status without institutional conflict).
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity_flat_control, diaspora_community, beneficiary,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity_flat_control, diaspora_community, payer).

% Reform Judaism and Reconstruction movements propose formally revising the binding status of sacrificial law. They argue that the law was historically contingent (belonged to the premodern context) and that binding status is renewable but not eternal. They have proposed exiting the paradox by revising the foundational claim that the law is eternally binding. This is a live institutional option (Reform communities have implemented this revision) but at high cost: Orthodox institutions treat revision as apostasy or theological error; practitioners who adopt this path often lose connection to Orthodox communities. The movements are excluded from the beneficiary set (they do not collect rent from the paradox) but are present in the constraint landscape as the primary agents proposing restructuring.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity_flat_control, reform_reconstruction_movements, excluded,
    organized, civilizational, mobile, global).

% The collective epistemic good of textual integrity and coherence. The maintenance of sacrificial law as binding despite recognized impossibility of performance compromises textual coherence: the text is treated as eternally valid while being reinterpreted in ways that contradict its literal meaning; the law's binding status is maintained while its performance conditions are acknowledged as permanently absent; the foundational claim (divine law is eternally binding) is held while the practical consequence (the law must be obeyed) is effectively nullified through substitution. The epistemic commons bears the cost of this incoherence: scholarship must work within contradiction; interpretation becomes increasingly elaborate and distant from literal meaning; practitioners develop compartmentalized cognition (knowing the law cannot be performed while treating it as binding). This is not an agent that collects or avoids extraction; it is a non-agent victim of the structural paradox.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity_flat_control, textual_epistemic_commons, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_continuity_flat_control, textual_epistemic_commons).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining Jewish communal identity, religious continuity, and textual heritage across geographic diaspora and centuries of displacement, in the absence of the Temple and priesthood that the founding law presupposes. The sacrifice law (now impossible to perform literally) becomes the mechanism for keeping the tradition alive across exile.
% TRANSFER_FUNCTION: Authority, respect, and interpretive legitimacy flow from the lay practitioner toward the interpretive authority class (rabbinic scholars and institutions). Lay practitioners must defer to rabbinic interpretation to determine what the binding law requires when performance is impossible. The interpretive authority class transfers back: legitimation of the tradition, maintenance of communal coherence, resolution of the paradox. But the transfer is asymmetric — the authority class collects interpretive rent (ongoing necessity) while practitioners bear the cognitive cost (binding obligation without means of performance).
% ABSENT_VOICES: Reform and historical-critical scholars who would argue that the binding status should be revised or abandoned are present in the institutional landscape but have low voice in Orthodox consensus-formation. Temple restoration movements and Jewish ethno-nationalist projects that would restore actual sacrifice are absent from contemporary diaspora consensus (though they emerge periodically in modern Israel). The individual lay practitioners whose experience the system constrains are diffusely present but lack organized voice in institutional jurisprudence — the constraint is maintained by and for the interpretive authority class, not by or for the practitioners who obey it.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (if the binding status of sacrificial law was formally revised or the tradition ceased maintaining the paradox), diaspora Jewish communities would rearrange: religious practice would shift away from paradoxical obligation structures; interpretive authority might be redistributed or relocated; communal identity would need alternative continuity mechanisms (possibly strengthened emphasis on other laws, cultural-ethnic identity, or explicit doctrinal revision). However, the constraint has now persisted so long that its disappearance might also leave communities without a clear identity anchor — the paradoxical obligation is woven into how contemporary practice maintains continuity with the premodern tradition. The verdict is contested because Orthodox institutions would argue that disappearance would mean loss of textual fidelity (the law remains binding), while Reform institutions would argue that restructuring (revising binding status) already constitutes disappearance of THIS paradoxical form, with minimal disruption because the underlying coordination function (communal continuity) can be maintained through alternative mechanisms.
% FOUNDING_PROBLEM: After the Second Temple was destroyed (70 CE), the physical conditions for performing sacrificial law ceased to exist. The challenge was how to maintain the law's authority and binding status while acknowledging the permanent impossibility of literal performance. The tradition needed a way to preserve connection to the foundational law while adapting practice to diaspora conditions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (maintaining continuity during exile, awaiting Temple restoration) was live for the first 500-1000 years (reflected in early rabbinic hope for restoration, liturgical prayers for restoration, interpretation of substitute rituals as temporary). By 1000 CE and certainly by the modern period, it was dead: no serious expectation of Temple restoration in the diaspora context exists; practice has been normalized around permanent substitutes; textual reverence is maintained through interpretive reframing rather than expectation of literal restoration. Contemporary rabbinic authorities acknowledge this (the Shulchan Aruch and later authorities treat substitutes as permanent practice, not temporary measures). Yet the original mandate — maintaining the law's binding status as if restoration were expected — is still formally maintained. This is the mandatrophy signature: the problem the constraint was built to solve no longer exists, but the constraint persists through institutional inertia and textual reverence. Corroboration: modern Jewish scholarship across traditions (Orthodox, Conservative, Reform, historical-critical) acknowledges that the founding problem (Temple restoration, awaiting performability) is no longer the active concern; the modern problem is how to maintain textual authority and communal identity while acknowledging the law's permanent unperformability.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity_flat_control, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY PRACTITIONER (SNARE) — Bound by interpretive authority to maintain ritual obligation as binding divine commandment. Cannot exit without abandoning religious identity and community membership. Physical impossibility of performance (no Temple, no altar) creates cognitive bind: the law must be obeyed, but obedience is materially impossible. Experiences maximum extraction — the obligation persists as a binding demand while the means of performance are foreclosed. Identity-locked because the practitioner's self-concept and community belonging are constituted through the tradition's framework, which treats the obligation as eternal.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 2: DIASPORA COMMUNITY (TANGLED ROPE) — Genuine coordination function: maintaining ritual continuity and communal identity across geographic dispersal and centuries of exile. But asymmetric extraction: the obligation to obey binding commandments persists while the means of compliance are materially foreclosed. Communities develop interpretive workarounds (prayer as substitute for sacrifice, textual recitation as substitute for Temple ritual) that maintain both the obligation AND a path to nominal compliance. This enables continued practice but institutionalizes the paradox — the law remains binding and unrevised while being reinterpreted into impossibility.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINIC JURISPRUDENCE (ROPE) — Pure coordination function for this perspective: maintaining textual continuity and interpretive authority across diaspora and centuries. The institution benefits from the paradox itself — the impossibility of direct performance creates ongoing demand for interpretive authority to adjudicate what the law requires in the absence of its performance conditions. The obligation's binding status is preserved (honoring the founding text) while interpretive latitude expands (redefining compliance through prayer, study, thought). Net beneficiary position — extraction runs toward the interpretive authority; they collect institutional rent from being the necessary translator between binding law and impossible performance.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM/CRITICAL MOVEMENTS (SCAFFOLD) — Organized actors (Reform Judaism, Reconstructionism, historical-critical scholarship) propose restructuring the obligation itself: either revising the binding status of sacrificial law (Reform: laws were historically contingent; binding status is renewable, not eternal) or recontextualizing it as historical artifact rather than binding commandment (historical-critical: law served its function in its time; modern practice can honor the tradition without maintaining the paradox). This perspective sees the constraint as temporary — a transition state between premodern literalism and modern revisionary frameworks. Has agency and a visible exit path: formal revision of what counts as binding. Theater is present but lower than the rabbinic workaround approach.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ORTHODOX INSTITUTIONAL CONSENSUS (PITON) — The most theatricalized perspective. Orthodox institutions maintain the formal binding status of sacrificial law while institutionalizing interpretive workarounds (prayer substitutes, memory recitation, study surrogates) that everyone knows do not constitute true sacrifice or Temple service. The constraint's function has atrophied — the binding law no longer directs actual performance (it cannot, physically) but persists through institutional inertia and textual reverence. Theater ratio is extremely high: the obligation is maintained as binding in legal compendium and commentary, performed through substitutes that satisfy neither the literal law nor practical efficacy, defended through elaborate interpretive apparatus that acknowledges the substitution while denying its incompleteness. Constrained exit: institutional leaders cannot revise the law's binding status without abandoning textual authority claims (career cost, legitimacy loss), so they maintain the performative ritual of obedience to an unperformable law.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DIVINE IMMUTABILITY (MOUNTAIN) — From the universalizing analytical perspective, this constraint appears as an immutable consequence of divine law's properties: if divine commandments are eternal and binding, and if a commandment was issued (sacrifice shall be performed), then that commandment must remain binding regardless of changed circumstances (Temple destroyed, diaspora, material impossibility). The constraint is seen as an inherent property of transcendent law itself — the logical consequence of combining eternal binding status with historical contingency. However, the base properties reveal a false summit: the 'immutability' is not a law of logic but a constructed institutional commitment. The engine will reclassify this as tangled rope or snare, revealing that what appears as divine necessity is actually a strategic choice to maintain textual authority while reinterpreting compliance.
constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sacrifice_obligation_continuity_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sacrifice_obligation_continuity_flat_control, TR),
    TR >= 0.70.

:- end_tests(sacrifice_obligation_continuity_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from the lay practitioner (binding obligation without means of compliance) and from the epistemic commons (textual coherence is compromised — the law's binding status is maintained despite universal recognition of its unperformability). The extraction is not maximal because genuine coordination function exists (diaspora community genuinely needs continuity mechanism) and because the interpretive authority structure is not purely extractive (they do solve the coordination problem, even if they also benefit from it). The value increased from 0.42 to 0.60 over the interval as the paradox became more institutionalized and performative. Suppression (0.62): Moderate-high. The suppression mechanisms are multiple: (1) identity lock — practitioners cannot exit without losing religious identity; (2) normative authority — the law is presented as binding and inalienable, with revision framed as apostasy; (3) interpretive monopoly — the rabbinic authority class controls what counts as 'complying' with the unperformable law, preventing the practitioner from declaring non-compliance or proposing alternatives. However, suppression is not total because the law itself remains open to scholarly interpretation and because movements like Reform Judaism have successfully proposed revision (though at high institutional cost). Theater ratio (0.78): High and rising. The constraint's performative character dominates contemporary practice: the obligation is maintained as binding in legal codes and halakhic compendia (Shulchan Aruch, etc.), performed through substitute rituals that everyone acknowledges are not true sacrifice, defended through interpretive apparatus that simultaneously acknowledges and denies the replacement. The theater increased over time as early rabbinic hope for Temple restoration faded and substitutes became institutionalized as permanent. Modern Orthodox institutional maintenance is almost entirely theatrical — the law persists as binding in books, enacted through surrogates, defended through exegesis, with no serious expectation of actual restoration.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural paradox produces radically different classifications from different positions. The lay practitioner locked into the identity sees a snare: binding obligation, impossible performance, no exit. The diaspora community sees coordination benefit (identity and continuity) mixed with extraction (obligation persists). The rabbinic authority sees pure coordination — solving the problem of maintaining legal tradition across exile. The Reform movements see a temporary problem with an available exit (revise binding status). The Orthodox institutional consensus sees inertial maintenance of a paradox everyone acknowledges (piton). The analytical observer risks seeing a natural law — the logical consequence of eternal binding status meeting historical contingency — but the beneficiary declarations and institutional analysis reveal this as a false summit. The perspectival gap is not just disagreement about whether the constraint is good; it is disagreement about what the constraint IS (mountain vs tangled rope vs snare). This disagreement is itself the diagnostic signal — the constraint that appears as natural law from the beneficiary's position appears as extraction from the victim's position.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from beneficiary/victim declarations plus power level plus exit options. The interpretive authority (institutional/arbitrage) benefits from ongoing necessity — low d, negative chi. The lay practitioner (powerless/identity_locked) is trapped in the obligation — high d, high chi. The diaspora community (moderate/constrained) experiences both benefits (coordination) and costs (extraction) with some agency — medium d, medium chi. This directionality profile explains why the constraint is tangled rope rather than pure rope or pure snare: there is genuine coordination function (diaspora community benefits from continuity mechanism) but also asymmetric extraction (lay practitioners bear obligation without means). The identity_locked exit option is critical for the lay practitioner perspective: structurally, the practitioner might have mobility (they could leave the community, adopt a different religion, revise their conception of binding obligation), but these exits are not viable from within the identity frame that the constraint constitutes. The practitioner's self-concept as religiously observant and community-embedded makes exit literally unthinkable — not because escape is impossible, but because 'escape' would mean ceasing to be who they understand themselves to be.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's original mandate (preserve sacrificial law's binding status and eternal character, honor the scriptural text, maintain connection to the premodern Temple system) has been substantially outlived by its function. The binding status is maintained, but not as a living legal obligation — rather as a theoretical commitment that everyone understands cannot be performed. The textual honor persists, but through interpretive reframing that domesticates the text's original meaning (sacrifice becomes prayer, Temple service becomes study). The connection to the premodern system exists only in memory and symbolic practice. Yet the constraint persists because the institutional beneficiaries (rabbinic authority class) maintain it through theater: the law remains in the codes, the obligation is preached as binding, the substitutes are performed, the exegesis is elaborate. This is the definition of mandatrophy — the original problem solved has been replaced by institutional inertia and theatrical maintenance. The solution (interpretive workaround enabling diaspora continuity) became a problem (cognitive bind and paradoxical obligation structure). Reform movements have attempted to resolve mandatrophy by revising the binding status explicitly (the law was historically contingent; binding status can be renewed but is not eternal). Orthodox institutions resist this resolution because it requires admitting that the textual authority is revisable. So mandatrophy is resolved only partially: acknowledged in scholarship and practice, but not structurally reformed in institutional consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_immutability_vs_institutional_choice,
    'Is the binding status of sacrificial law an inherent property of divine law (immutable, necessary, transcendent) or a constructed institutional commitment that could be revised without abandoning the tradition?',
    'Comparative textual analysis: did other ancient legal systems revise binding laws when conditions changed (Roman ius civile, Islamic fiqh, Hellenistic legal revision)? Did Jewish tradition revise other laws deemed binding? If yes to either: immutability is not inherent but chosen. Theological analysis: do foundational texts restrict the authority to revise law, or only restrict WHO can revise?',
    'If divine immutability: mountain classification correct, constraint is natural law. If institutional choice: false summit confirmed, constraint is tangled rope or snare (extraction through maintaining paradox). Classification hinges on this omega.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_immutability_vs_institutional_choice, conceptual, 'Whether binding status is immutable divine property or institutional choice').

omega_variable(
    functional_substitution_sufficiency,
    'Do prayer, study, and textual recitation constitute genuine performance of the sacrificial obligation, or do they represent a workaround that maintains the appearance of obedience while acknowledging non-performance?',
    'Textual-historical analysis: did medieval and early modern authorities argue that substitutes ARE sacrifice (genuine equivalence) or that substitutes REPLACE sacrifice temporarily (acknowledged imperfection)? Psychological ethnography: how do contemporary practitioners experience the substitution — as satisfying the obligation or as coping mechanism? If authorities acknowledged imperfection: suppression mechanism is internalized (practitioners know they are not truly obeying, but cannot exit).',
    'If genuine equivalence: snare classification weakens (less cognitive dissonance, less identity lock). If acknowledged imperfection: snare classification confirmed (cognitive bind is intentional institutional product). Affects identity_locked diagnosis — practitioners may be locked less by identity fusion and more by explicit institutional denial of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_substitution_sufficiency, empirical, 'Whether ritual substitutes genuinely perform the obligation or maintain appearance of obedience').

omega_variable(
    narrative_obligation_persistence_rationale,
    'Why is the binding status of sacrificial law maintained as a live legal obligation across 2000 years despite universal recognition of physical impossibility? What institutional function does the paradox serve?',
    'Historical institutional analysis: did rabbinic institutions resist revision attempts? Did textual authority depend on treating ALL laws as binding? Did competing interpretive schools (Karaites, Sadducees if reconstructed, Reform) gain authority by proposing revision? Comparative analysis: how did other traditions (Islam post-Hijra, Christianity post-Temple) handle binding laws that became impossible or obsolete?',
    'If maintained for interpretive authority: beneficiary extraction (institutional class benefits from ongoing necessity) is confirmed. If maintained for textual reverence alone: extraction is lower (constraint may be rope rather than tangled rope). If maintained as theodicy (divine law still perfect despite diaspora): mythological function replaces extraction function, constraint may be scaffold with sunset at religious modernization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_obligation_persistence_rationale, conceptual, 'Institutional rationale for maintaining paradoxical obligation').

omega_variable(
    identity_lock_dissolution_trajectory,
    'When practitioners encounter intellectual challenges to the obligation''s binding status, do they maintain identity lock through compartmentalization, or does the lock dissolve enabling exit?',
    'Cohort ethnography: do practitioners exposed to historical-critical scholarship maintain the obligation (identity lock persists) or revise it (identity lock dissolved)? Do they compartmentalize (scholarly doubt + ritual practice)? Longitudinal: do reformed/secular practitioners report identity loss when dissolving the obligation, or identity gain (cognitive coherence, intellectual integrity)?',
    'If identity lock persists through intellectual challenge: suppression and identity_locked classification confirmed. If lock dissolves: practitioners may move from identity_locked to constrained (high-cost but surmountable exit). If compartmentalization succeeds: neither suppression nor identity lock fully captures the mechanism — practitioners are aware of paradox but tolerate it. Affects whether constraint is snare (lock is inescapable) or tangled rope (lock is escapable but costly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dissolution_trajectory, empirical, 'Persistence of identity lock across intellectual challenges to obligation''s binding status').

omega_variable(
    false_summit_detector_beneficiary_presence,
    'Given that the mountain perspective (divine immutability) claims emerges_naturally: false while beneficiaries are explicitly declared, is the constraint a genuine natural law or a false summit naturalizing institutional extraction?',
    'This omega is triggered by the FSM schema gate (mountain + beneficiaries requires omegas). The question is whether the constraint''s appearance as natural law is a structural property of transcendent law or a cover story. Does the institution benefit from treating the obligation as unchangeable and immutable? Yes (institutional authority, interpretive rent). Does the institution actively defend the immutability frame against revision proposals? Yes. Does the immutability serve the institution''s interests? Yes. These point to false summit: the paradox is maintained because it benefits the interpretive authority class, which naturalizes it as divine necessity.',
    'False summit confirmed: constraint is tangled rope or snare, not mountain. The beneficiary extraction is the primary function; textual reverence is the justification. The engine''s false summit detector will reclassify this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_detector_beneficiary_presence, conceptual, 'FSM confirmation: beneficiary-maintained paradox naturalizes as divine law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity_flat_control, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soc_theater_0, sacrifice_obligation_continuity_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(soc_theater_500, sacrifice_obligation_continuity_flat_control, theater_ratio, 500, 0.68).
narrative_ontology:measurement(soc_theater_1000, sacrifice_obligation_continuity_flat_control, theater_ratio, 1000, 0.78).
narrative_ontology:measurement(soc_theater_1500, sacrifice_obligation_continuity_flat_control, theater_ratio, 1500, 0.8).

% Extraction over time
narrative_ontology:measurement(soc_extract_0, sacrifice_obligation_continuity_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(soc_extract_500, sacrifice_obligation_continuity_flat_control, base_extractiveness, 500, 0.54).
narrative_ontology:measurement(soc_extract_1000, sacrifice_obligation_continuity_flat_control, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(soc_extract_1500, sacrifice_obligation_continuity_flat_control, base_extractiveness, 1500, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(soc_suppress_0, sacrifice_obligation_continuity_flat_control, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(soc_suppress_500, sacrifice_obligation_continuity_flat_control, suppression_requirement, 500, 0.58).
narrative_ontology:measurement(soc_suppress_1000, sacrifice_obligation_continuity_flat_control, suppression_requirement, 1000, 0.62).
narrative_ontology:measurement(soc_suppress_1500, sacrifice_obligation_continuity_flat_control, suppression_requirement, 1500, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity_flat_control, 0.12).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity_flat_control, temple_restoration_theology).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity_flat_control, halakhic_interpretation_authority).

% DUAL FORMULATION NOTE:
% This constraint is the master structure upstream of two domain-specific constraints: temple restoration theology (the theological doctrine that the Temple will be restored and sacrifices performed) is downstream — it depends on maintaining sacrifice law as binding; halakhic interpretation authority (the institutional claim that rabbinic interpretation is authoritative for determining binding law) is also downstream — it depends on the paradox of binding law without performance conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_continuity_flat_control, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
