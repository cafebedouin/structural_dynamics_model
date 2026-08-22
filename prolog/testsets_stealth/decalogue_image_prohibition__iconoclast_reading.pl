% ============================================================================
% CONSTRAINT STORY: decalogue_image_prohibition__iconoclast_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [REPUDIATED_AT_INTERVAL_END]
% ============================================================================

:- module(constraint_decalogue_image_prohibition__iconoclast_reading, []).

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
 *   constraint_id: decalogue_image_prohibition__iconoclast_reading
 *   human_readable: Iconoclast Prohibition on Religious Imagery (Byzantine Enforcement, 726-843)
 *   domain: theology/religious_authority/visual_culture
 *
 * SUMMARY:
 *   Between 726 and 843 the Byzantine imperial state enforced a categorical
 *   ban on religious imagery: every material representation used in worship
 *   was declared idolatry, images were destroyed or whitewashed, and
 *   veneration was punished by oath, mutilation, exile, and death. This story
 *   instantiates the iconoclast READING of the decalogue_image_prohibition
 *   kernel — the claim that the commandment forbids all material mediation of
 *   the holy — as an enforced arrangement, not as a theological claim in the
 *   abstract. Per the epsilon-invariance principle the kernel decomposes into
 *   a constraint family: the iconodule reading (honor through images to their
 *   prototypes is legitimate; the Incarnation sanctifies matter) and the
 *   moderate reading (three-dimensional statuary forbidden, two-dimensional
 *   images regulated) are separate constraints with their own victim sets and
 *   their own epsilon values, linked through the network. The claim/metric
 *   gap is deliberate: the arrangement is CLAIMED as tangled_rope — a genuine
 *   coordination program (uniform aniconic worship, commandment compliance as
 *   the court reads it) entangled with asymmetric extraction (imperial
 *   monopolization of religious form, confiscation, persecution) — while the
 *   authored metrics describe the arrangement's actual operation across its
 *   full arc, including its two collapses and its reinstatement.
 *   Base_properties characterize the arrangement across its operative life,
 *   weighted toward its enforcement phases; the measurement series shows the
 *   complete trajectory including the suspensions.
 *
 * KEY AGENTS:
 *   - centralizing_imperial_authority: agenda-setter (institutional/arbitrage) — issues edicts, convenes councils, commands enforcement; collects uniform religious form and confiscated wealth
 *   - iconoclast_state_clergy: beneficiary with enforcement duties (institutional/constrained) — staffs the Hieria hierarchy; careers valid only while the ban holds
 *   - theme_army_settlers: beneficiary (organized/constrained) — soldier-settlers bound to iconoclast emperors by land and loyalty
 *   - icon_painters: payer (moderate/constrained) — vocation criminalized; mutilation, exile, forced recantation
 *   - monastic_communities: payer (organized/identity_locked) — institutional core of image veneration; raided, whipped, exiled
 *   - image_devotion_laity: payer (powerless/constrained) — devotional life criminalized; oaths, informers, hidden icons
 *   - diaspora_iconodule_theologians: payer (moderate/arbitrage) — anathematized abroad, beyond enforcement reach
 *   - roman_papacy: excluded (institutional/arbitrage) — rejects the ban from outside enforcement reach
 *   - church_historians: observer (analytical/analytical) — later councils and historians assess from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, 0.74).
domain_priors:suppression_score(decalogue_image_prohibition__iconoclast_reading, 0.82).
domain_priors:theater_ratio(decalogue_image_prohibition__iconoclast_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(decalogue_image_prohibition__iconoclast_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(decalogue_image_prohibition__iconoclast_reading, tangled_rope).
narrative_ontology:human_readable(decalogue_image_prohibition__iconoclast_reading, "Iconoclast Prohibition on Religious Imagery (Byzantine Enforcement, 726-843)").
narrative_ontology:topic_domain(decalogue_image_prohibition__iconoclast_reading, "theology/religious_authority/visual_culture").

domain_priors:requires_active_enforcement(decalogue_image_prohibition__iconoclast_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(decalogue_image_prohibition__iconoclast_reading, 'f2790708-ab49-4531-b4f4-73cd5f19d95b').
narrative_ontology:cs_kernel_codification('f2790708-ab49-4531-b4f4-73cd5f19d95b', fixed_text).
narrative_ontology:cs_authority_grounding('f2790708-ab49-4531-b4f4-73cd5f19d95b', extraction).
narrative_ontology:cs_interpretation_layer_present('f2790708-ab49-4531-b4f4-73cd5f19d95b').
narrative_ontology:cs_reading_relation('f2790708-ab49-4531-b4f4-73cd5f19d95b', decalogue_image_prohibition__iconodule_reading, forecloses).
narrative_ontology:cs_reading_relation('f2790708-ab49-4531-b4f4-73cd5f19d95b', decalogue_image_prohibition__moderate_iconoclast_reading, forecloses).
narrative_ontology:cs_axiom('f2790708-ab49-4531-b4f4-73cd5f19d95b', foundational, all_cultic_imagery_is_idolatry).
narrative_ontology:cs_axiom_status(all_cultic_imagery_is_idolatry, holdable).
narrative_ontology:cs_axiom_grounding('f2790708-ab49-4531-b4f4-73cd5f19d95b', all_cultic_imagery_is_idolatry, theological).
narrative_ontology:cs_axiom('f2790708-ab49-4531-b4f4-73cd5f19d95b', foundational, incarnation_grants_no_image_license).
narrative_ontology:cs_axiom_status(incarnation_grants_no_image_license, holdable).
narrative_ontology:cs_axiom_grounding('f2790708-ab49-4531-b4f4-73cd5f19d95b', incarnation_grants_no_image_license, theological).
narrative_ontology:cs_reference_frame('f2790708-ab49-4531-b4f4-73cd5f19d95b', plain_sense_aniconic_commandment).
narrative_ontology:cs_drift_state('f2790708-ab49-4531-b4f4-73cd5f19d95b', post_triumph_of_orthodoxy, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('f2790708-ab49-4531-b4f4-73cd5f19d95b', '').
narrative_ontology:cs_kernel_id(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, iconoclast_state_clergy).
narrative_ontology:constraint_beneficiary(decalogue_image_prohibition__iconoclast_reading, theme_army_settlers).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, icon_painters).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, monastic_communities).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, image_devotion_laity).
narrative_ontology:constraint_victim(decalogue_image_prohibition__iconoclast_reading, diaspora_iconodule_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The emperor and court issue the prohibition edicts, convene the councils that canonize them (Hieria, 754), and command the army and church hierarchy to destroy images and punish veneration. The ban gives the court one authorized religious form across the empire, a doctrinal instrument against the wealthy and semi-autonomous monasteries, and confiscable property to reward soldiers. Enforcement intensity moves with dynastic politics: intensified under Constantine V, suspended under Irene's regency in 787, reinstated by Leo V in 815, abandoned by Theodora in 843.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Bishops and court theologians consecrated under the Hieria canons staff the enforcement hierarchy: they preside over image destruction, administer the oaths imposed on clergy and laity, and fill sees vacated by deposed image-venerating bishops. Their consecrations and careers are valid only while the settlement holds; a restoration unseats them, as the councils of 787 and 843 in fact did.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, iconoclast_state_clergy, beneficiary,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(decalogue_image_prohibition__iconoclast_reading, iconoclast_state_clergy, agenda_setter).

% Soldiers settled in the Anatolian and Balkan themes, some on lands confiscated from monasteries under Constantine V's resettlements. Their emperors' religious policy was tied to their pay, land, and loyalty; iconoclast emperors drew their strongest support from the army, and army discontent was the principal risk any restoration had to manage.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, theme_army_settlers, beneficiary,
    organized, biographical, constrained, regional).

% Workshop painters, mosaicists, and illuminators whose vocation is the production of sacred images. Under the ban their works are whitewashed or burned, commissions vanish, and practitioners face mutilation — the painter Lazaros had his hands burned before resuming work in secret — exile, or forced recantation. Flight to Rome, the Levant, or image-friendly courts is possible but costs them their guild standing and homeland.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, icon_painters, payer,
    moderate, biographical, constrained, regional).

% Monasteries are the institutional core of image veneration: their liturgy, contemplative practice, and charity are organized around icons of Christ and the saints. Under the ban they are raided, their icons burned, their monks whipped through the Mese, exiled, or killed — the persecutions of 765-766 targeted monastic leadership specifically. Their property is confiscable. Exit would mean dissolving the community's rule of life itself, which is bound to the images.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, monastic_communities, payer,
    organized, generational, identity_locked, continental).

% Ordinary households and parishioners whose devotion runs through household icons, church frescoes, and the visual culture of feast and relic. They swear imposed oaths against veneration, hide images, and risk informers and penalties. Their attachment persists underground and resurfaces whenever enforcement relaxes.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, image_devotion_laity, payer,
    powerless, biographical, constrained, regional).

% Theologians writing from outside imperial jurisdiction — John of Damascus from Umayyad Damascus above all — who compose the defense of images that circulates back into the empire. The Hieria council anathematizes John by name in absentia, but the ban cannot reach him; his treatises become the canon of the opposing position.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, diaspora_iconodule_theologians, payer,
    moderate, generational, arbitrage, continental).

% The Roman see rejects the ban from the start: Gregory II and Gregory III refuse compliance, withhold ratification from Hieria, and break communion with the iconoclast patriarchs. It shelters refugee monks and painters and lends its authority to the resistance, at little cost to itself, since imperial enforcement cannot reach Rome.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, roman_papacy, excluded,
    institutional, generational, arbitrage, continental).

% Later councils and ecclesial historians — the acts of 787, the 842/843 restoration synod and Synodicon, and the historiographic tradition that follows — assess the iconoclast settlement from outside its enforcement, preserving testimony from every seat and ultimately recording the reading as anathema.
narrative_ontology:constraint_stakeholder(decalogue_image_prohibition__iconoclast_reading, church_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(decalogue_image_prohibition__iconoclast_reading, centralizing_imperial_authority).
narrative_ontology:fixing_cost_class(decalogue_image_prohibition__iconoclast_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes worship across the empire's churches and armies around an aniconic norm: one authorized liturgical form, uniform clerical conformity, and a bright boundary between Christian practice and the image-using cults of the surrounding world, as the court reads the Decalogue's second commandment to require.
% TRANSFER_FUNCTION: Moves religious-form authority from image-producing workshops, monasteries, and devotional laity to the imperial center and its consecrated hierarchy; moves material wealth through confiscations of monastic property to the treasury and to soldier-settlers in the themes; moves obedience through imposed oaths from clergy and laity to the court.
% ABSENT_VOICES: The four eastern patriarchs and the Roman see were absent from Hieria, which claimed ecumenical rank with none of the five patriarchs present. Icon painters and devotional laity had no seat anywhere in the settlement; their objections survive chiefly as the record of their punishments. Image-venerating monks were present only as defendants and martyrs.
% DISAPPEARANCE_RATIONALE: The empire's liturgy, army settlement economy, clergy careers, and art production were organized around the ban. Its removal reconstitutes the image economy, restores monastic property and autonomy, unseats the Hieria hierarchy, and reopens Rome's communion — which is approximately what happened, twice (787 and 843), each time forcing wholesale rearrangement.
% FOUNDING_PROBLEM: Whether material images used in worship are the idolatry the Decalogue's second commandment forbids: the commandment's plain text appeared to prohibit them, and images seemed to invite the veneration practices of the surrounding image-using cultures.
% FOUNDING_PROBLEM_CORROBORATION: Inside the benefiting parties, the Council of Hieria (754) and the army-backed hierarchy attest the problem in the iconoclast framing. Outside that set: John of Damascus, writing from Damascus, attests the idolatry concern as real but answers it the opposite way; the Roman sees of Gregory II and III attest the ban as a novel imposition rather than the commandment's plain demand; and Nicaea II (787), with the 843 Synodicon, attests from outside the iconoclast beneficiary set that the problem was resolved by distinguishing honor from worship rather than by abolition. No party outside the imperial-enforcement set corroborates the iconoclast framing as the commandment's necessary reading.
narrative_ontology:disappearance_verdict(decalogue_image_prohibition__iconoclast_reading, world_rearranges).
narrative_ontology:founding_problem_status(decalogue_image_prohibition__iconoclast_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(decalogue_image_prohibition__iconoclast_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(decalogue_image_prohibition__iconoclast_reading, 'none', 1).
narrative_ontology:epsilon_provenance(decalogue_image_prohibition__iconoclast_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(decalogue_image_prohibition__iconoclast_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(decalogue_image_prohibition__iconoclast_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(decalogue_image_prohibition__iconoclast_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.74) because enforcement destroyed livelihoods and communities and concentrated religious-form authority and confiscated wealth at the court, while the coordination the ban purchased (uniform worship) could in principle have been delivered by the moderate reading at far lower cost — the categorical scope is what makes the arrangement extractive rather than merely costly. Suppression (0.82) is the load-bearing wall: the ban persisted only while the army and hierarchy actively destroyed images and punished veneration, and it collapsed twice, in 787 and 843, the moment enforcement stopped. Theater is moderate-low (0.25): destruction and punishment were functionally real, but the arrangement produced theatrical legitimacy — Hieria claiming ecumenical rank with none of the five patriarchs, staged recantations, the public parading of monks — peaking around 754-770. Accessibility_collapse is moderate (0.55): the alternative never fully collapsed — it survived in monasteries, households, Rome, and Damascus — but within the empire's public space it was driven underground at peak enforcement. Resistance (0.8) is near the ceiling: organized monastic resistance, diaspora theology, Roman refusal, popular attachment that resurfaced whenever enforcement relaxed, and finally two restorations imposed from the palace itself. The measurement series run on one shared grid of eight points and model a full cycle: rise (726-770), collapse (787), reinstatement and second rise (815-830), final collapse (843). The oscillation is itself signal, not noise: the arrangement could not reproduce itself through conviction alone and required re-imposition by successive dynasties — an intermittent-reinforcement structure in which each reimposition extracted renewed oaths and renewed confiscations. Suppression is authored as the raw structural coercion the arrangement applied; it is not scaled by power or scope — the engine scales only extractiveness, by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the imperial seat the arrangement is a uniformity program the court built and could reverse at will — the emperor's exit is arbitrage, so even heavy costs register as policy choices rather than burdens. From the monastic seat the same arrangement is an existential assault: identity-locked exit means the ban cannot be complied with except by dissolving what the community is, so the monastic seat should compute the arrangement at or near full severity. From the diaspora theologian's seat the ban is anathema without cost — arbitrage exit damps what the arrangement can take to nearly nothing, which is why the empire's anathemas against John of Damascus bought it nothing. The laity sit between: powerless and constrained, they paid in oaths and hidden icons, and their diffuse coalition — popular attachment to images — proved to be the resistance resource on which both restorations rode. Same-level differentiation is sharpest between the two payer seats of comparable standing: monastic communities inside the empire (identity-locked, reachable) and diaspora theologians outside it (arbitrage, unreachable) hold the same doctrinal position and compute very different costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the court (agenda-setter and collector — d near the beneficiary end), the Hieria clergy (sees and careers), and the army settlers (land and their emperors' legitimacy). Victims: painters, monasteries, and laity (d near the target end, amplified for the identity-locked monastic seat, damped for the untouchable diaspora seat by its arbitrage exit). The papacy is excluded rather than coordinated: its absence from Hieria is part of the arrangement's design, and its structural distance is what let it anchor the resistance. No directionality overrides were authored — the derivation from declared beneficiaries, victims, power, and exit produces the correct relationships, including the diaspora damping and the amplification for trapped and identity-locked payers. Scope runs continental at the center and regional at the payers, so the engine's scope amplification applies most where verification was hardest: in the villages and monasteries farthest from Constantinople.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whether material imagery is the idolatry the commandment forbids — was the dispute itself, and it was settled against this reading by the framework that hosted it: Nicaea II distinguished honor from worship, and the 843 restoration anathematized Hieria. The classification keeps the two components distinct. The coordination component (uniform aniconic worship, boundary maintenance against image-using cults) was real, and it is what the moderate sibling reading could have delivered at lower cost; the extraction component (monopolized religious form, confiscated monastic wealth, broken monastic autonomy) is what the categorical scope added on top. Calling the whole arrangement pure extraction would erase the sincere theological program that made enforcement sustainable for a century; calling it pure coordination would erase the martyrs. The tangled_rope claim holds both at once, and the sincerity-versus-extraction omega is the lever that would move the classification if resolved: if enforcement patterns show the monastic wealth and autonomy, not image use per se, were the operative targets, the arrangement slides toward pure extraction; if the theological program dominates the record, the coordination component strengthens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_delta,
    'How would the victim and beneficiary sets and the classification change under the sibling readings — iconodule (dulia permitted) and moderate (two-dimensional images regulated) — of the same kernel?',
    'Author the sibling readings as separate constraint stories and compare computed classifications across the kernel family; the structural deltas should fall out of their differing beneficiary/victim declarations.',
    'Under the iconodule reading the victim set shrinks toward those giving images worship proper (latria) and the beneficiary set shifts toward image-producing guilds and monasteries; under the moderate reading icon painters largely exit the victim set. Epsilon falls substantially in both siblings relative to this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta, conceptual, 'Committer structure: one kernel, three readings; this story instantiates the iconoclast reading and its classification is not transferable to the siblings.').

omega_variable(
    foreclosure_vs_coexistence,
    'Do the iconoclast and iconodule core premises logically exclude each other within a single ecclesial commitment framework (forecloses), or do they merely coexist as live factional positions (coexists_with)?',
    'Test whether any single framework can canonize both ''all material mediation in worship is idolatry'' and ''honor through images to prototypes is legitimate''; the 787 and 843 adjudications are the natural experiment — the framework was forced to pick and anathematized one reading.',
    'If coexists_with is correct, the kernel family models as a pluralist dispute with no forced resolution; if forecloses is correct, the readings are mutually anathematizing and the framework''s adjudication is structurally forced, which changes how drift and terminal states compute for every member of the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence, conceptual, 'Whether the authored forecloses edges to both siblings are logically forced or merely factional.').

omega_variable(
    sincerity_vs_extraction,
    'Was the iconoclast program primarily sincere theological enforcement (obedience to the commandment as the court read it) or primarily imperial consolidation (monopolizing religious form, breaking monastic wealth and autonomy)?',
    'Comparative analysis of enforcement patterns: if enforcement targeted wealth and autonomy (monastic confiscations, leadership persecutions of 765-766) more than image use per se, consolidation dominates; if image destruction proceeded even where no wealth followed, conviction dominates.',
    'If primarily sincere, the arrangement moves toward coordination-with-costs; if primarily consolidating, toward pure extraction. The tangled_rope classification holds only while both components are real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sincerity_vs_extraction, empirical, 'The coordination/extraction entanglement that decides between tangled_rope and its neighbors.').

omega_variable(
    army_constituency_dependence,
    'How much of the ban''s persistence rested on the army''s material stake (confiscated monastic lands, imperial legitimacy) versus theological conviction among soldiers?',
    'Compare policy stability under emperors with and without army settlement programs (Constantine V''s resettlements versus Irene''s regency), and trace army sentiment at each reversal.',
    'If the material stake dominates, the beneficiary structure is more concentrated and the arrangement sits closer to pure extraction; if conviction dominates, the coordination component is stronger and the arrangement more stable than its two collapses suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(army_constituency_dependence, empirical, 'Whether the beneficiary seat''s attachment to the ban was pecuniary, doctrinal, or both.').

omega_variable(
    interlude_suspension_vs_latency,
    'During the 787-815 iconodule interlude, was the iconoclast constraint genuinely suspended, or merely latent — surviving in army culture, hierarchy memory, and diaspora sympathy, ready for reimposition?',
    'Trace institutional continuity of iconoclast personnel and army sentiment across the interlude; Leo V''s reversal within a few years of regaining leverage suggests latency rather than death.',
    'If latent, the constraint''s true interval is continuous 726-843 with varying intensity, the 787 dip is enforcement collapse rather than constraint death, and the lifecycle reads as one long entangled-rope arc rather than two shorter regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interlude_suspension_vs_latency, empirical, 'Whether the 787-815 interlude suspends the constraint or only its enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(decalogue_image_prohibition__iconoclast_reading, 726, 843).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iconoclast_ban_tr_t726, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 726, 0.18).
narrative_ontology:measurement(iconoclast_ban_tr_t741, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 741, 0.25).
narrative_ontology:measurement(iconoclast_ban_tr_t754, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 754, 0.35).
narrative_ontology:measurement(iconoclast_ban_tr_t770, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 770, 0.4).
narrative_ontology:measurement(iconoclast_ban_tr_t787, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 787, 0.1).
narrative_ontology:measurement(iconoclast_ban_tr_t815, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 815, 0.3).
narrative_ontology:measurement(iconoclast_ban_tr_t830, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 830, 0.38).
narrative_ontology:measurement(iconoclast_ban_tr_t843, decalogue_image_prohibition__iconoclast_reading, theater_ratio, 843, 0.12).

% Extraction over time
narrative_ontology:measurement(iconoclast_ban_be_t726, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 726, 0.45).
narrative_ontology:measurement(iconoclast_ban_be_t741, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 741, 0.62).
narrative_ontology:measurement(iconoclast_ban_be_t754, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 754, 0.72).
narrative_ontology:measurement(iconoclast_ban_be_t770, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 770, 0.78).
narrative_ontology:measurement(iconoclast_ban_be_t787, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 787, 0.15).
narrative_ontology:measurement(iconoclast_ban_be_t815, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 815, 0.55).
narrative_ontology:measurement(iconoclast_ban_be_t830, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 830, 0.66).
narrative_ontology:measurement(iconoclast_ban_be_t843, decalogue_image_prohibition__iconoclast_reading, base_extractiveness, 843, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(iconoclast_ban_su_t726, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 726, 0.5).
narrative_ontology:measurement(iconoclast_ban_su_t741, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 741, 0.65).
narrative_ontology:measurement(iconoclast_ban_su_t754, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 754, 0.75).
narrative_ontology:measurement(iconoclast_ban_su_t770, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 770, 0.85).
narrative_ontology:measurement(iconoclast_ban_su_t787, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 787, 0.1).
narrative_ontology:measurement(iconoclast_ban_su_t815, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 815, 0.6).
narrative_ontology:measurement(iconoclast_ban_su_t830, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 830, 0.7).
narrative_ontology:measurement(iconoclast_ban_su_t843, decalogue_image_prohibition__iconoclast_reading, suppression_requirement, 843, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(decalogue_image_prohibition__iconoclast_reading, identity_coordination).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__iconodule_reading).
narrative_ontology:affects_constraint(decalogue_image_prohibition__iconoclast_reading, decalogue_image_prohibition__moderate_iconoclast_reading).

% DUAL FORMULATION NOTE:
% Constraint family from epsilon-invariance decomposition: the colloquial label 'the iconoclasm dispute' covers three structurally distinct constraints instantiating one kernel (the second commandment's scope). This story, the iconoclast reading, carries the highest epsilon when enforced: its categorical scope criminalizes the entire image economy and its devotional base, with the imperial center as beneficiary. The iconodule reading (matter sanctified by the Incarnation; the latria/dulia distinction) carries low epsilon — its enforcement after 843 protects the image economy rather than attacking it. The moderate reading sits between: three-dimensional statuary banned, two-dimensional images regulated. Upstream/downstream structure: this reading's enforcement (726-787, 815-843) created the conditions — persecution, diaspora theology, Roman rupture — under which the iconodule reading was systematized and ultimately canonized; the sibling story should record the reverse edge and its own reading_relations (the iconodule reading likewise forecloses this one within a single framework, and the 843 anathemas are its receipt).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
